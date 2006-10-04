unit ScanEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, db, dbtables, ComCtrls;

type
  TScanEditDB = class(TEdit)
  private
    { Déclarations privées }
    ffieldname: string;
    fdatasource: tdatasource;
    FOnChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    procedure EventOnChange(Sender: TObject);
    procedure EventKeyPress(Sender: TObject; var Key: Char);
  protected
    { Déclarations protégées }
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  public
    { Déclarations publiques }
    constructor create(aowner: tcomponent); override;
  published
    { Déclarations publiées }
    property FieldName: string read ffieldname write ffieldname;
    property DataSource: tdatasource read fdatasource write fdatasource;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScanEditLV = class(TEdit)
  private
    { Déclarations privées }
    FListView: TListView;
    FOnChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    procedure EventOnChange(Sender: TObject);
    procedure EventKeyPress(Sender: TObject; var Key: Char);
  protected
    { Déclarations protégées }
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  public
    { Déclarations publiques }
    constructor create(aowner: tcomponent); override;
  published
    { Déclarations publiées }
    property ListView: TListView read FListView write FListView;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScanEditTV = class(TEdit)
  private
    { Déclarations privées }
    FTreeView: TTreeView;
    FOnChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    procedure EventOnChange(Sender: TObject);
    procedure EventKeyPress(Sender: TObject; var Key: Char);
  protected
    { Déclarations protégées }
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  public
    { Déclarations publiques }
    constructor create(aowner: TComponent); override;
  published
    { Déclarations publiées }
    property TreeView: TTreeView read FTreeView write FTreeView;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Medi@ Kit', [TScanEditDB, TScanEditLV, TScanEditTV]);
end;

constructor tscaneditDB.create(aowner: tcomponent);
begin
  inherited create(aowner);
  inherited OnChange := EventOnChange;
  inherited OnKeyPress := EventKeyPress;
end;

procedure TScanEditDB.EventOnChange(Sender: TObject);
var
  p: integer;
  bm: TBookmark;
begin
  if (Trim(FieldName) <> '') and Assigned(datasource) and Assigned(datasource.DataSet) then begin
    p := SelStart;
    inherited OnChange := nil;
    with datasource.DataSet do begin
      DisableControls;
      bm := GetBookmark;
      try
        First;
        if Locate(FieldName, text, [locaseinsensitive, lopartialkey])
          then text := fieldbyname(fieldname).AsString
          else GotoBookmark(bm);
        SelStart := p;
        sellength := length(text);
      finally
        FreeBookmark(bm);
        EnableControls;
        inherited onchange := EventOnChange;
      end;
    end;
  end;
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TScanEditDB.EventKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Sender, Key);
  if key = #8 then begin
    selstart := selstart - 1;
    sellength := length(text);
  end;
end;

constructor TScanEditLV.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  inherited OnChange := EventOnChange;
  inherited OnKeyPress := EventKeyPress;
end;

procedure TScanEditLV.EventOnChange(Sender: TObject);
var
  p: Integer;
  li, oli: TListItem;
begin
  if Assigned(ListView) then begin
    inherited OnChange := nil;
    try
      p := SelStart;
      oli := ListView.Selected;
      li := ListView.FindCaption(0, Text, True, True, False);
      if Assigned(li) and (Text <> '') then begin
        Text := li.Caption;
        oli := li;
      end;
      ListView.Selected := oli;
      if Assigned(oli) then oli.MakeVisible(False);
      SelStart := p;
      SelLength := Length(Text);
    finally
      inherited OnChange := EventOnChange;
    end;
  end;
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TScanEditLV.EventKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Sender, Key);
  if key = #8 then begin
    selstart := selstart - 1;
    sellength := length(text);
  end;
end;

constructor tscaneditTV.create(aowner: tcomponent);
begin
  inherited create(aowner);
  inherited OnChange := EventOnChange;
  inherited OnKeyPress := EventKeyPress;
end;

function FindCaptionTV(TV: TTreeView; StartIndex: Integer; Value: String; Partial, Inclusive, Wrap: Boolean): TTreeNode;
var
  I: Integer;
  S: string;
  Item: TTreeNode;
begin
  Result := nil;
  Value := UpperCase(Value);
  if Inclusive then Dec(StartIndex);
  for I := StartIndex + 1 to TV.Items.Count - 1 do begin
    Item := TV.Items[I];
    if (Item <> nil) then
      s := UpperCase(Item.Text);
      if (not Partial and (s = Value)) or
         (Partial and (Copy(s, 1, Length(Value)) = Value)) then begin
        Result := Item;
        Exit;
      end;
  end;
  if Wrap then begin
    if Inclusive then Inc(StartIndex);
    for I := 0 to StartIndex - 1 do begin
      Item := TV.Items[I];
      if (Item <> nil) then
        s := UpperCase(Item.Text);
        if (not Partial and (s = Value)) or
           (Partial and (Copy(s, 1, Length(Value)) = Value)) then begin
          Result := Item;
          Exit;
        end;
    end;
  end;
end;

procedure TScanEditTV.EventOnChange(Sender: TObject);
var
  p: integer;
  li, oli: TTreeNode;
begin
  if Assigned(TreeView) then begin
    inherited OnChange := nil;
    try
      p := SelStart;
      oli := TreeView.Selected;
      li := FindCaptionTV(TreeView, 0, Text, True, True, False);
      if assigned(li) and (text <> '') then begin
        text := li.Text;
        oli := li;
      end;
      TreeView.Selected := oli;
      if Assigned(oli) then oli.MakeVisible;
      SelStart := p;
      SelLength := Length(Text);
    finally
      inherited onchange := EventOnChange;
    end;
  end;
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TScanEditTV.EventKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Sender, Key);
  if key = #8 then begin
    selstart := selstart - 1;
    sellength := length(text);
  end;
end;

end.
