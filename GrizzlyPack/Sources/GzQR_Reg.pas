unit GzQR_Reg;

{$I GrizzlyDefine.inc}

interface

uses GzConsts;

procedure Register;

implementation

uses
  SysUtils, Classes, QuickRpt, TypInfo, AUtils, QRCtrls, AClasses,
  QRExpander, QRCustomGrid, DB, GZDesign
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};


{ QRExpander }

type
  TAffectedBandEditor = class(TComponentProperty)
  public
     procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TAffectedBandEditor.GetValues(Proc: TGetStrProc);
type TGetStrFunc = function(const S: string): Integer of object;
var BaseReport: TQuickRep;
    L: TStringList;
    F: TGetStrFunc;
    i: Integer;
begin
  { On n'accepte que les Bandes appartenant au MasterReport }
  BaseReport:= TQRExpander(GetComponent(0)).MasterReport;
  if BaseReport = nil then Exit;
     { Recensement des bandes }
  L:= TStringList.Create; F:= L.Add;
  Designer.GetComponentNames(GetTypeData(GetPropType), TGetStrProc(F));
  for i:= 0 to L.Count - 1 do
     if TQRCustomBand(Designer.GetComponent(L[i])).ParentReport = BaseReport then
        Proc(L[i]);
  L.Free;
end;

{ SetValue : Il faut penser à n'accepter que ce qui est acceptable ... }

type
  TExpandingQRProperty = class(TComponentProperty)
  public
     procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TExpandingQRProperty.GetValues(Proc: TGetStrProc);
type TGetStrFunc = function(const S: string): Integer of object;
var Expander: TQRExpander;
    Current: TQRPrintable;
    L: TStringList;
    F: TGetStrFunc;
    i: Integer;
    function IsBandChild(Item: TQRPrintable): Boolean;
    var i: Integer;
    begin
       Result:= False; i:= 0;
       while (i < Expander.AffectedBand.ControlCount) and (not Result) do
       begin
           Result:= Result or (Expander.AffectedBand.Controls[i] = Item);
           Inc(i);
       end;
    end;
begin
  { On n'accepte que les QRPrintables appartenant au MasterReport ET à AffectedBand }
  Expander:= TQRExpander(GetComponent(0));
  if (Expander.AffectedBand = nil) or (Expander.MasterReport = nil) then Exit;
     { Recensement des QRPrintables }
  L:= TStringList.Create; F:= L.Add;
  Designer.GetComponentNames(GetTypeData(GetPropType), TGetStrProc(F));
  for i:= L.Count - 1 downto 0 do
  begin
     Current:= TQRPrintable(Designer.GetComponent(L[i]));
     if (Current.ParentReport = Expander.MasterReport) and IsBandChild(Current) and
          IsCorrectClass(Current, [TQRDBText, TQRLabel, TQRDBRichText, TQRExpr]) then
        Proc(L[i]);
  end;
  L.Free;
end;

type
  TAffectedQRProperty = class(TComponentProperty)
  public
     procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TAffectedQRProperty.GetValues(Proc: TGetStrProc);
type TGetStrFunc = function(const S: string): Integer of object;
var Expander: TQRExpander;
    Current: TQRPrintable;
    L: TStringList;
    F: TGetStrFunc;
    i: Integer;
    function IsBandChild(Item: TQRPrintable): Boolean;
    var i: Integer;
    begin
       Result:= False; i:= 0;
       while (i < Expander.AffectedBand.ControlCount) and (not Result) do
       begin
           Result:= Result or (Expander.AffectedBand.Controls[i] = Item);
           Inc(i);
       end;
    end;
    function IsAffected(Item: TQRPrintable): Boolean;
    var i: Integer;
    begin
       Result:= Expander.ExpandingQRComponent = Item;
       if not Result then
       begin
          i:= 0;
          while (i < Expander.AffectedQRComponents.Count - 1) and (not Result) do
          begin
             Result:= TExpandItem(Expander.AffectedQRComponents.Items[i]).AffectedItem = Item;
             Inc(i);
          end;
       end;
    end;
begin
  { On n'accepte que les QRPrintables appartenant au MasterReport ET à AffectedBand }
  Expander:= TExpandCollection(TCollectionItem(GetComponent(0)).Collection).Expander;
  if (Expander.AffectedBand = nil) or (Expander.MasterReport = nil) then Exit;
     { Recensement des QRPrintables }
  L:= TStringList.Create; F:= L.Add;
  Designer.GetComponentNames(GetTypeData(GetPropType), TGetStrProc(F));
  for i:= L.Count - 1 downto 0 do
  begin
     Current:= TQRPrintable(Designer.GetComponent(L[i]));
     if (Current.ParentReport = Expander.MasterReport) and IsBandChild(Current) and (not IsAffected(Current)) then
        Proc(L[i]);
  end;
  L.Free;
end;

type
  TQRExpanderEditor = class(TComponentEditor)
  private
    {$IFDEF GZ_D6}
    FEditor: IProperty;
    {$ELSE}
    FEditor: TPropertyEditor;
    {$ENDIF}
  protected
    {$IFDEF GZ_D6}
    procedure GetEditor(const Editor: IProperty);
    {$ELSE}
    procedure GetEditor(Editor: TPropertyEditor);
    {$ENDIF}
  public
    procedure Edit; override;
  end;

{$IFDEF GZ_D6}
procedure TQRExpanderEditor.GetEditor(const Editor: IProperty);
begin
  if Editor.GetName = 'AffectedQRComponents' then
    FEditor:= Editor;
end;

procedure TQRExpanderEditor.Edit;
var L: IDesignerSelections;
begin
  L:= TDesignerSelections.Create;
  L.Add(Component);
  GetComponentProperties(L, [tkClass], Designer, GetEditor);
  FEditor.Edit;
end;
{$ELSE}
procedure TQRExpanderEditor.GetEditor(Editor: TPropertyEditor);
begin
  if Editor.GetName = 'AffectedQRComponents' then
    FEditor:= Editor;
end;

procedure TQRExpanderEditor.Edit;
var L: TDesignerSelectionList;
begin
  L:= TDesignerSelectionList.Create;
  try
    L.Add(Component);
    GetComponentProperties(L, [tkClass], Designer, GetEditor);
    FEditor.Edit;
  finally
    L.Free;
  end;
end;
{$ENDIF}

{ QRCustomGrid }

type
  TQRGzGridColumnFieldProperty = class(TFieldProperty)
  public
    function GetDataSet: TDataSet; override;
  end;

function TQRGzGridColumnFieldProperty.GetDataSet: TDataSet;
begin
  Result:= TQRGridColumn(GetComponent(0)).Grid.DataSet;
end;

type
  TQRGzGridEditor = class(TComponentEditor)
  private
    {$IFDEF GZ_D6}
    FEditor: IProperty;
    {$ELSE}
    FEditor: TPropertyEditor;
    {$ENDIF}
  protected
    {$IFDEF GZ_D6}
    procedure GetEditor(const Editor: IProperty);
    {$ELSE}
    procedure GetEditor(Editor: TPropertyEditor);
    {$ENDIF}
  public
    procedure Edit; override;
  end;

{$IFDEF GZ_D6}
procedure TQRGzGridEditor.GetEditor(const Editor: IProperty);
begin
  if Editor.GetName = 'Columns' then
    FEditor:= Editor;
end;

procedure TQRGzGridEditor.Edit;
var L: IDesignerSelections;
begin
  L:= TDesignerSelections.Create;
  L.Add(Component);
  GetComponentProperties(L, [tkClass], Designer, GetEditor);
  FEditor.Edit;
end;
{$ELSE}
procedure TQRGzGridEditor.GetEditor(Editor: TPropertyEditor);
begin
  if Editor.GetName = 'Columns' then
    FEditor:= Editor;
end;

procedure TQRGzGridEditor.Edit;
var L: TDesignerSelectionList;
begin
  L:= TDesignerSelectionList.Create;
  try
    L.Add(Component);
    GetComponentProperties(L, [tkClass], Designer, GetEditor);
    FEditor.Edit;
  finally
    L.Free;
  end;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents(SGrizzlyQR, [TQRExpander]);
  RegisterPropertyEditor(TQRCustomBand.ClassInfo, TQRExpander, 'AffectedBand', TAffectedBandEditor);
  RegisterPropertyEditor(TQRPrintable.ClassInfo, TQRExpander, 'ExpandingQRComponent', TExpandingQRProperty);
  RegisterPropertyEditor(TQRPrintable.ClassInfo, TExpandItem, 'AffectedItem', TAffectedQRProperty);
  RegisterComponentEditor(TQRExpander, TQRExpanderEditor);

  RegisterComponents(SGrizzlyQR, [TQRCustomGrid]);
  RegisterPropertyEditor(TypeInfo(string), TQRGridColumn, 'DataField', TQRGzGridColumnFieldProperty);
  RegisterComponentEditor(TQRCustomGrid, TQRGzGridEditor);
end;

end.
