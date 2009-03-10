unit QRExpander;

{$I GrizzlyDefine.inc}

interface

uses QuickRpt, QRCtrls, Classes, ComCtrls, RichEdit;

{
  Définition :
    A un rapport SPECIFIQUE
    A une bande DONNÉE
    A un composant QR DONNÉ appartenant à la BANDE
    On associe des composants QR appartenant à la BANDE
    Pour chacun de ces composants, on définie si on doit :
            1 - Modifier la POSITION (Top)
            2 - Modifier la HAUTEUR (Height)

    Le composant s'occupe alors des choses suivantes :
      Dans l'événement BEFORE PRINT de la BANDE, on fait :
        - Stockage de la HAUTEUR de la BANDE
        - Stockage de la HAUTEUR du composant DONNÉ
        - Stockage des HAUTEURS & POSITIONS des composants choisis
        Définition de la nouvelle HAUTEUR du composant DONNÉ
          ( Si c'est un DB, on vérifie si c'est un BLOB (=> Assign), sinon := .AsString
            Sinon on vérifie s'il y a une propriété Caption
            Sinon on vérifie si c'est un TQRExpr
            Sinon on vérifie s'il y a une propriété Lines ou Strings = TStrings )
        MODIFICATION des HAUTEURS ou POSITIONS de :
          1 - La BANDE (Height) => On fait NewPage ou NewColumn si nécessaire
          2 - Le COMPOSANT à étendre (Height)
          3 - Les COMPOSANTS associés (Top OU Height)
      Dans l'événement AFTER PRINT de la BANDE, on fait :
        - Restauration de l'ancienne HAUTEUR de la BANDE
        - Restauration de l'ancienne HAUTEUR du composant DONNÉ
        - Restauration des anciennes HAUTEURS & POSITIONS des composants choisis
}

type
  TQRExpander = class(TComponent)
  private
     FEnabled: Boolean; 
     FOldECHeight: Double;
     FOldBandHeight: Double;
     {}
     FAffectedComponents: TCollection;
     FExpandingComponent: TQRPrintable;
     {}
     FMasterReport: TQuickRep;
     FMasterBand: TQRCustomBand;
     {}
     FOldBandBP: TQRBandBeforePrintEvent;
     FOldBandAP: TQRBandAfterPrintEvent;
     procedure SetExpandingComponent(Component: TQRPrintable);
     procedure SetAffectedComponents(Components: TCollection);
     procedure SetMasterBand(Band: TQRCustomBand);
     procedure SetMasterReport(Report: TQuickRep);
     {}
     procedure HookBandEvents;
     procedure UnHookBandEvents;
     {}
     procedure BeforePrintBand(Sender: TQRCustomBand; var PrintBand: Boolean);
     procedure AfterPrintBand(Sender: TQRCustomBand; BandPrinted: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     function CalcAdjustment: Double;
     procedure AdjustSizes(Adjustment: Double);
     procedure StoreSizes;
     procedure RestoreSizes;
     procedure AdjustSize;
  published
     property ExpandingQRComponent: TQRPrintable read FExpandingComponent write SetExpandingComponent;
     property AffectedQRComponents: TCollection read FAffectedComponents write SetAffectedComponents;
     property AffectedBand: TQRCustomBand read FMasterBand write SetMasterBand;
     property MasterReport: TQuickRep read FMasterReport write SetMasterReport;
     property Enabled: Boolean read FEnabled write FEnabled;
  end;

  function SupposedHeight(Report: TQuickRep; QRPrintable: TQRPrintable): Double;
  function IsCorrectClass(Instance: TObject; Classes: array of TClass): Boolean;
  
type
  TAffectedSize = (asTop, asHeight);
  TExpandItem = class(TCollectionItem)
  private
    FItem: TQRPrintable;
    FAffectedSize: TAffectedSize;
    FOldSize: Double;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Item: TPersistent); override;
    property OldSize: Double read FOldSize write FOldSize;
  published
    property AffectedItem: TQRPrintable read FItem write FItem;
    property AffectedSize: TAffectedSize read FAffectedSize write FAffectedSize;
  end;

  TExpandCollection = class(TCollection)
  private
    FExpander: TQRExpander;
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Expander: TQRExpander read FExpander write FExpander;
  end;

implementation

uses SysUtils, Windows, DB, AUtils, Printers, TypInfo;

var FRichEdit: TRichEdit;


  function TExpandItem.GetDisplayName: string;
  begin
    if Assigned(AffectedItem) then
      Result:= AffectedItem.Name
    else
      Result:= inherited GetDisplayName;
  end;

  procedure TExpandItem.Assign(Item: TPersistent);
  begin
    inherited Assign(Item);
    if Item is TExpandItem then
    begin
      AffectedItem:= TExpandItem(Item).AffectedItem;
      AffectedSize:= TExpandItem(Item).AffectedSize;
      OldSize:= TExpandItem(Item).OldSize;
    end;
  end;

function TExpandCollection.GetOwner: TPersistent;
begin
  Result:= Expander;
end;

procedure TExpandCollection.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TExpandCollection then Expander:= TExpandCollection(Source).Expander;
end;

{$IFNDEF GZ_D5}
type
  TPLabel = class(TQRCustomLabel)
  public
    property FormattedLines;
  end;
{$ENDIF}

function SupposedHeight(Report: TQuickRep; QRPrintable: TQRPrintable): Double;
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY: Integer;
  VSize: Extended;
  OldMapMode: Integer;
  function AssignText(QRComponent: TQRPrintable): Boolean;
  var
    P: PPropInfo;
    S: string;
    DataSet: TDataSet;
  begin
    Result:= True;
      { Vérifie si c'est un DB }
    P:= GetPropInfo(QRComponent.ClassInfo, 'DataSet');
    if P <> nil then
    begin
      DataSet:= Pointer(GetOrdProp(QRComponent, P));
      P:= GetPropInfo(QRComponent.ClassInfo, 'DataField');
      if P <> nil then
      begin
        S:= GetStrProp(QRComponent, P);
        if DataSet.FieldByName(S).IsBlob then
          FRichEdit.Lines.Assign(DataSet.FieldByName(S))
        else
          FRichEdit.Lines.Text:= DataSet.FieldByName(S).AsString;
//        Value:= DataSet.FieldByName(S).AsString;
      end else
        Result:= False;
    end else { Si ce n'est pas un DB, c'est peut-être un "Caption" }
    begin
      P:= GetPropInfo(QRComponent.ClassInfo, 'Caption');
      if P <> nil then
      begin
        S:= GetStrProp(QRComponent, P);
        FRichEdit.Lines.Text:= S;
      end else if QRComponent is TQRExpr then
      begin { Si ce n'est pas un "Caption", c'est alors un TQRExpr ? }
         FRichEdit.Lines.Text:= TQRExpr(QRComponent).Value.strResult
//        Value:= TQRExpr(QRComponent).Value.strResult;
      end else begin
        P:= GetPropInfo(QRComponent.ClassInfo, 'Lines');
        if P <> nil then
          FRichEdit.Lines.Text:= TStrings(GetOrdProp(QRComponent, P)).Text
//          Value:= TStrings(GetOrdProp(QRComponent, P)).Text
        else
          Result:= False;
      end;
    end;
    if Result then
    begin
//      P:= GetPropInfo(QRComponent.ClassInfo, 'Font');
//      if P <> nil then
//        Font:= TFont(GetOrdProp(QRComponent, P)));
      Exit;
      P:= GetPropInfo(QRComponent.ClassInfo, 'Font');
      if P <> nil then
        FRichEdit.Font.Assign(TPersistent(GetOrdProp(QRComponent, P)));
    end;
  end;
begin
  Result:= 0;
  FRichEdit.Parent:= QRPrintable;
  try
    { Assignation du texte }
    if not AssignText(QRPrintable) then Exit;
    if not Assigned(Report.QRPrinter.Canvas) then
      Exit;
    (*{$IFNDEF VER130}
    if QRPrintable is TQRCustomLabel then
    begin
      Result:= QRPrintable.ParentReport.TextHeight(TQRCustomLabel(QRPrintable).Font, 'W') * QRPrintable.Zoom div 100 + 1;
      VSize:= QRPrintable.Height;
      QRPrintable.Height:= Round(Result);
      Result:= QRPrintable.Size.Height;
      TPLabel(QRPrintable).FormatLines;
      QRPrintable.Height:= Round(VSize);
      if TPLabel(QRPrintable).FormattedLines.Count <> 0 then
        Result:= TPLabel(QRPrintable).FormattedLines.Count * Result;
      Exit;
    end else {$ENDIF}*)
    { Calcule la taille nécessaire au texte pour s'afficher ... }
    with Range do
    begin
      hdc := Report.QRPrinter.Canvas.Handle;
      hdcTarget := hdc;
      LogX := GetDeviceCaps(hdc, LOGPIXELSX);
      LogY := GetDeviceCaps(hdc, LOGPIXELSY);
      rc.Left := Report.QRPrinter.XPos(0) * 1440 div LogX;
      rc.Top := Report.QRPrinter.YPos(0) * 1440 div LogY;
      rc.Right := Report.QRPrinter.XPos(QRPrintable.Size.Width) * 1440 div LogX;
      rc.Bottom := Report.QRPrinter.YPos(QRPrintable.Size.Height) * 1440 div LogY;
      rcPage := rc;
      LastChar := 0;
      MaxLen := FRichEdit.GetTextLen;
      chrg.cpMax := -1;
      chrg.cpMin := LastChar;

      OldMapMode := SetMapMode(hdc, MM_TEXT);

      VSize := QRPrintable.Size.Height;

      LastChar := SendMessage(FRichEdit.Handle, EM_FORMATRANGE, 0, Longint(@Range));

      while (LastChar < MaxLen) and (LastChar <> -1) do
      begin
        VSize := VSize + 10;
        rc.Bottom := Report.QRPrinter.YPos(VSize) * 1440 div LogY;
        LastChar := SendMessage(FRichEdit.Handle, EM_FORMATRANGE, 0, Longint(@Range));
      end;
    end;
    SetMapMode(Report.QRPrinter.Canvas.Handle, OldMapMode);
    SendMessage(FRichEdit.Handle, EM_FORMATRANGE, 0, 0);
    Result:= VSize;
  finally
    FRichEdit.Parent:= nil;
  end;
end;

constructor TQRExpander.Create(AOwner: TComponent);
begin
  FAffectedComponents:= TExpandCollection.Create(TExpandItem);
  TExpandCollection(FAffectedComponents).Expander:= Self;
  inherited Create(AOwner);
  FEnabled:= True;
end;

destructor TQRExpander.Destroy;
begin
  UnHookBandEvents;
  FAffectedComponents.Free;
  FAffectedComponents := nil;
  inherited Destroy;
end;

procedure TQRExpander.StoreSizes;
var i: Integer;
    P: TObject;
    Item: TExpandItem;
begin
  for i:= 0 to AffectedQRComponents.Count - 1 do
  begin
    Item:= TExpandItem(AffectedQRComponents.Items[i]);
    P:= Pointer(GetOrdPropByName(Item.AffectedItem, 'Size'));
    case Item.AffectedSize of
      asTop: Item.OldSize:= GetFloatPropByName(P, 'Top');
      asHeight: Item.OldSize:= GetFloatPropByName(P, 'Height');
    end;
  end;
  FOldECHeight:= ExpandingQRComponent.Size.Height;
  FOldBandHeight:= AffectedBand.Size.Height;
end;

function TQRExpander.CalcAdjustment: Double;
begin
  Result:= SupposedHeight(MasterReport, ExpandingQRComponent) - ExpandingQRComponent.Size.Height;
end;

procedure TQRExpander.AdjustSize;
begin
  AdjustSizes(CalcAdjustment);
end;

procedure TQRExpander.AdjustSizes(Adjustment: Double);
var i: Integer;
    P: TObject;
    Item: TExpandItem;
begin
//  if AffectedBand.AlignToBottom then
//    MasterReport.CurrentY:= MasterReport.CurrentY - Round(Adjustment) else
  if not AffectedBand.CanExpand(Adjustment) then
    MasterReport.NewColumn;
  AffectedBand.Size.Height:= FOldBandHeight + Adjustment;
  {}
  for i:= 0 to AffectedQRComponents.Count - 1 do
  begin
    Item:= TExpandItem(AffectedQRComponents.Items[i]);
    P:= Pointer(GetOrdPropByName(Item.AffectedItem, 'Size'));
    case Item.AffectedSize of
      asTop: SetFloatPropByName(P, 'Top', Item.OldSize + Adjustment);
      asHeight: SetFloatPropByName(P, 'Height', Item.OldSize + Adjustment);
    end;
  end;
  ExpandingQRComponent.Size.Height:= FOldECHeight + Adjustment;
end;

procedure TQRExpander.RestoreSizes;
var i: Integer;
    P: TObject;
    Item: TExpandItem;
begin
  for i:= 0 to AffectedQRComponents.Count - 1 do
  begin
    Item:= TExpandItem(AffectedQRComponents.Items[i]);
    P:= Pointer(GetOrdPropByName(Item.AffectedItem, 'Size'));
    case Item.AffectedSize of
      asTop: SetFloatPropByName(P, 'Top', Item.OldSize);
      asHeight: SetFloatPropByName(P, 'Height', Item.OldSize);
    end;
  end;
  ExpandingQRComponent.Size.Height:= FOldECHeight;
  AffectedBand.Size.Height:= FOldBandHeight;
end;

procedure TQRExpander.BeforePrintBand(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  if Assigned(FOldBandBP) then FOldBandBP(Sender, PrintBand);
  if Enabled then
  begin
    StoreSizes;
    AdjustSize;
  end;
end;

procedure TQRExpander.AfterPrintBand(Sender: TQRCustomBand; BandPrinted: Boolean);
begin
  if Enabled then RestoreSizes;
  if Assigned(FOldBandAP) then FOldBandAP(Sender, BandPrinted);
end;

  { Settings }
procedure TQRExpander.SetMasterBand(Band: TQRCustomBand);
begin
{ On surcharge immédiatement les événements BeforePrint & AfterPrint de la bande }
  if Band <> FMasterBand then
  begin
    if not (csLoading in ComponentState) then
    begin
      FExpandingComponent:= nil;
      AffectedQRComponents.Clear; { Reset TOUT }
    end;
    FMasterBand:= Band;
    if (not (csDesigning in ComponentState)) then
    begin
      if Band = nil then
        UnHookBandEvents
      else
        HookBandEvents;
    end;
  end;
end;

procedure TQRExpander.SetMasterReport(Report: TQuickRep);
begin
  if Report <> FMasterReport then
  begin
    if not (csLoading in ComponentState) then SetMasterBand(nil);
    FMasterReport:= Report;
  end;
end;

procedure TQRExpander.SetAffectedComponents(Components: TCollection);
begin
  FAffectedComponents.Assign(Components);
end;

function IsCorrectClass(Instance: TObject; Classes: array of TClass): Boolean;
var i: Integer;
begin
  Result:= Instance = nil;
  if not Result then
    for i:= 0 to High(Classes) do
      Result:= Result or (Instance is Classes[i]);
end;

procedure TQRExpander.SetExpandingComponent(Component: TQRPrintable);
begin
  { TQRLabel ou TQRDBText ou TQRDBRichText ou TQRExpr }
  if (FExpandingComponent <> Component) or (Component = nil) then
  begin
    FExpandingComponent:= Component;
    if not (csLoading in ComponentState) then FAffectedComponents.Clear; { Reset TOUT }
  end;
end;

procedure TQRExpander.HookBandEvents;
begin
  if csDesigning in ComponentState then
    Exit;
  if AffectedBand = nil then
  begin
    FOldBandBP:= nil;
    FOldBandAP:= nil;
  end else begin
    FOldBandBP:= AffectedBand.BeforePrint;
    FOldBandAP:= AffectedBand.AfterPrint;
    AffectedBand.BeforePrint:= BeforePrintBand;
    AffectedBand.AfterPrint:= AfterPrintBand;
  end;
end;

procedure TQRExpander.UnHookBandEvents;
var
  OldBP, CurBP: TQRBandBeforePrintEvent;
  OldAP, CurAP: TQRBandAfterPrintEvent;
begin
  if not Assigned(AffectedBand) then
  begin
    FOldBandBP:= nil;
    FOldBandAP:= nil;
    Exit;
  end;
  OldBP:= AffectedBand.BeforePrint;
  CurBP:= FOldBandBP;
  if @OldBP = @CurBP then
  begin
    AffectedBand.BeforePrint:= FOldBandBP;
    FOldBandBP:= nil;
  end;
  OldAP:= AffectedBand.AfterPrint;
  CurAP:= FOldBandAP;
  If @OldAP = @CurAP then
  begin
    AffectedBand.AfterPrint:= FOldBandAP;
    FOldBandAP:= nil;
  end;
end;

procedure TQRExpander.Notification(AComponent: TComponent; Operation: TOperation);
var i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = MasterReport then
    begin
      AffectedBand := nil;
      MasterReport := nil;
    end
    else if AComponent = AffectedBand then AffectedBand:= nil
    else if AComponent = ExpandingQRComponent then ExpandingQRComponent:= nil
    else begin
      if Assigned(FAffectedComponents) then
        for i:= 0 to AffectedQRComponents.Count - 1 do
          if TExpandItem(AffectedQRComponents.Items[i]).AffectedItem = AComponent then
          begin
            TExpandItem(AffectedQRComponents.Items[i]).AffectedItem:= nil;
          end;
    end;
  end;
end;


initialization
  FRichEdit:= TRichEdit.Create(nil);

finalization
  FRichEdit.Free;

end.

