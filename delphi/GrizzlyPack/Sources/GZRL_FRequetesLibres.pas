unit GZRL_FRequetesLibres;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FiltExpt, Db, Grids, DBGrids, PManager, StdCtrls, Buttons,
  DBInfo, Printers, FGOrdLB, DataSetStreamer, CheckLst, GZRL_Composant,
  RxGrdCpt, VDataSet, ExitPnl, ToolWin, ComCtrls, HintCtrl, Clipbrd;

type
  TModeReqLibre = (mrQuery, mrVDataSet);
  TModeEditionModele = (emModele, emSnapShot);
  TCSVFormat = record
    FieldSeparator: string;
    LineBreak: string;
    QuoteChar: string;
  end;

  TFenRequetesLibres = class(TForm)
    DSRequete: TDataSource;
    GridResultat: THDBGrid;
    PanelManager1: TPanelManager;
    MPanelOptions: TMPanel;
    MPanelFilter: TMPanel;
    FilterExpert: TFilterExpert;
    MPanelColonnes: TMPanel;
    LBProps: TCheckListBox;
    FontDialog1: TFontDialog;
    MPanelEtat: TMPanel;
    RGOrientation: TRadioGroup;
    Panel1: TPanel;
    Label3: TLabel;
    EditTitre: TEdit;
    BtnFontInfos: TSpeedButton;
    BtnFontTitres: TSpeedButton;
    Panel2: TPanel;
    EditFieldName: TEdit;
    Label1: TLabel;
    ChBVisible: TCheckBox;
    Label2: TLabel;
    EditDisplayLabel: TEdit;
    Panel3: TPanel;
    LBOrderBy: TOrderByListBox;
    Label4: TLabel;
    Panel4: TPanel;
    BtnChangeOrder: TSpeedButton;
    btnEnleverColTri: TSpeedButton;
    Panel5: TPanel;
    BtnShowAll: TSpeedButton;
    SpeedButton4: TSpeedButton;
    BtnHideAll: TSpeedButton;
    FVDataSet: TVirtualDataSet;
    PanelBtnModeles: TPanel;
    btnEditerModeles: TSpeedButton;
    btnEnregistrer: TSpeedButton;
    btnCharger: TSpeedButton;
    btnInitEdition: TSpeedButton;
    PanelBtnSnapShots: TPanel;
    btnSaveSnapShot: TSpeedButton;
    Panel6: TPanel;
    btnAppliquer: TSpeedButton;
    btnExport: TSpeedButton;
    btnImpression: TSpeedButton;
    btnLoadSnapShot: TSpeedButton;
    btnEditSnapShots: TSpeedButton;
    PanelRecordCount: TPanel;
    ExitPanel: TExitPanel;
    Panel7: TPanel;
    ToolBarAction: TToolBar;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel8: TPanel;
    btnFermer: TSpeedButton;
    btnCopierGrille: TSpeedButton;
    procedure FilterExpertApply(Sender: TObject; Selection: string);
    procedure LBPropsClick(Sender: TObject);
    procedure EditDisplayLabelKeyPress(Sender: TObject; var Key: Char);
    procedure ChBVisibleClick(Sender: TObject);
    procedure EditDisplayLabelExit(Sender: TObject);
    procedure btnImpressionClick(Sender: TObject);
    procedure btnFermerClick(Sender: TObject);
    procedure BtnFontInfosClick(Sender: TObject);
    procedure GridResultatColEnter(Sender: TObject);
    procedure GridResultatTitleClick(Column: TColumn);
    procedure LBPropsClickCheck(Sender: TObject);
    procedure MPanelColonnesResize(Sender: TObject);
    procedure LBOrderByDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBOrderByDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure btnEnleverColTriClick(Sender: TObject);
    procedure LBPropsDblClick(Sender: TObject);
    procedure LBOrderByOrderChange(Sender: TObject; Ascending: Boolean);
    procedure BtnChangeOrderClick(Sender: TObject);
    procedure BtnShowAllClick(Sender: TObject);
    procedure btnInitEditionClick(Sender: TObject);
    procedure btnChargerClick(Sender: TObject);
    procedure btnEnregistrerClick(Sender: TObject);
    procedure btnEditerModelesClick(Sender: TObject);
    procedure btnAppliquerClick(Sender: TObject);
    procedure GridResultatDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FontDialog1Show(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
    procedure btnSaveSnapShotClick(Sender: TObject);
    procedure btnLoadSnapShotClick(Sender: TObject);
    procedure btnEditSnapShotsClick(Sender: TObject);
    procedure FilterExpertEditWithButtonClick(Sender: TObject;
      FieldName: String; var Value, InternalValue: String);
    procedure FilterExpertFillValues(Sender: TObject; FieldName: String;
      List: TStrings);
    procedure FilterExpertSelectField(Sender: TObject; FieldName: String;
      var EditKind: TEditKind);
    procedure ExitPanelOkClick(Sender: TObject);
    procedure ExitPanelCancelClick(Sender: TObject);
    procedure PanelRecordCountDblClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure btnCopierGrilleClick(Sender: TObject);
  private
    FEditFontTitle: Boolean;
    FTitreFontDialog: string;
    FQuery: TDataSet;
    FCountQuery: TDataSet;
    FDefaultOrderBy: string;
    FDefaultSQL: TStrings;
    FDataSetModeles: TDataSetModeles;
    FDataSetSnapShots: TDataSetModeles;
    FReqLibre: TRequetesLibres;
    FMode: TModeReqLibre;
    FTitle: string;
    FRecordCount: Integer;
    {}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {}
    function GetDataSet: TDataSet;
    {}
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    {}
    procedure AutoAssignerDateModifModeles(DataSet: TDataSet);
    procedure AutoAssignerDateModifSnapShots(DataSet: TDataSet);
    procedure SetMode(const Value: TModeReqLibre);
    procedure SaveColumnInfo(AStream: TStream);
    procedure ReadColumnInfo(AStream: TStream);
  protected
    function GetSQL(const DataSet: TDataSet): string;
    procedure SetSQL(const DataSet: TDataSet; const SQL: string);
    {}
    procedure CSVExport(FileName: string; CSVFormat: TCSVFormat);
    procedure ConstExport(FileName: string);
    procedure CopyExport;
    {}
    procedure NouvelleEdition;
    procedure RemplirProps;
    procedure SetProps(AFi: TField);
    procedure UpDateProps;
    {}
    function StoreColWidths : TStream; //La function renvoie un TStream...
    procedure RestoreColWidths(AStream : TStream); //...que la procédure se charge de libérer.
    {}
    function GetRecordCount: Integer;
  public
    property DefaultSQL: TStrings read FDefaultSQL write FDefaultSQL;
    property DefaultOrderBy: string read FDefaultOrderBy write FDefaultOrderBy;
    {}
    property DataSetModeles: TDataSetModeles read FDataSetModeles write FDataSetModeles;
    property DataSetSnapShots: TDataSetModeles read FDataSetSnapShots write FDataSetSnapShots;
    {}
    property RequeteLibre: TRequetesLibres read FReqLibre write FReqLibre;
    {}
    property Query: TDataSet read FQuery write FQuery;
    property CountQuery: TDataSet read FCountQuery write FCountQuery;
    property VDataSet: TVirtualDataSet read FVDataSet write FVDataSet;
    {}
    property DataSet: TDataSet read GetDataSet;
    {}
    property Mode: TModeReqLibre read FMode write SetMode;
    {}
    property Title: string read FTitle write FTitle;

  end;

var
  FenRequetesLibres: TFenRequetesLibres;

implementation

{$R *.DFM}

uses FGDBUtil, AClasses, AFiles, FGUtils, TypInfo, Math, StrUtils,
    GZRL_FSelModeleEtat, GZRL_FEditModeleEtat, GZRL_FEditModelesEtats,
    GZRL_FChoixExport, GZRL_RRequetesLibres, QRCustomGrid, QRUtils;

const
  StreamVersion = 1;

procedure TFenRequetesLibres.FormShow(Sender: TObject);
  procedure ConstruireFieldListFilterExpert;
  var i: Integer;
      F: TField;
      S1, S2: string;
  begin
    FilterExpert.FieldList.Clear;
    for i:= 0 to Query.FieldCount - 1 do
    begin
      F:= Query.Fields[i];
      S1:= F.DisplayName;
      S2:= F.FieldName;
      FilterExpert.FieldList.Add(Format('%s=%s', [S1, S2]));
    end;
  end;
  procedure CreerChampsVDataSet;
  var i: Integer;
      F: TField;
      C: TFieldClass;
  begin
    VDataSet.FieldDefs.Clear;
    for i:= 0 to Query.FieldCount - 1 do
    begin
      C:= TFieldClass(Query.Fields[i].ClassType);
      F:= C.Create(VDataSet);
      GenericAssignEx(Query.Fields[i], F);
      F.DataSet:= VDataSet;
    end;
    VDataSet.Open;
  end;
  procedure CreateButtons(Items: TActionRequetesLibres);
  var NewButton: TSpeedButton;
  begin
    if Items.Visible then
    begin
      NewButton:= TSpeedButton.Create(self);
      NewButton.Glyph.Assign(Items.Glyph);
      NewButton.Hint:= Items.Hint;
      NewButton.NumGlyphs:= Items.NumGlyph;
      NewButton.Parent:= ToolBarAction;
      NewButton.OnClick:= Items.OnClick;
      NewButton.Enabled := Items.Enabled;
    end;
  end;

var i: Integer;
begin
  ExitPanel.Visible:= FReqLibre.ExitPanelVisible;
  {}
  Mode:= mrQuery;
  {}
  for i:= 0 to FReqLibre.CustomActions.Count - 1 do
    CreateButtons(TActionRequetesLibres(FReqLibre.CustomActions.Items[i]));
  ConstruireFieldListFilterExpert;
  {}
  NouvelleEdition;
  RemplirProps;
  {}
  Query.Open;
  {}
  if Assigned(DataSetModeles.DataSet) then
    DataSetModeles.DataSet.BeforePost:= AutoAssignerDateModifModeles;
  if Assigned(DataSetSnapShots.DataSet) then
    DataSetSnapShots.DataSet.BeforePost:= AutoAssignerDateModifSnapShots;
  if Assigned(DataSetSnapShots.DataSet) then
    CreerChampsVDataSet;
  {}
  PanelBtnModeles.Visible:= Assigned(DataSetModeles.DataSet);
  PanelBtnSnapShots.Visible:= Assigned(DataSetSnapShots.DataSet);

  if Assigned(RequeteLibre.OnShow) then
    RequeteLibre.OnShow(Sender);

end;

procedure TFenRequetesLibres.btnFermerClick(Sender: TObject);
begin
  Close;
end;

{ Divers }

procedure TFenRequetesLibres.MPanelColonnesResize(Sender: TObject);
begin
  LBProps.Width:= MPanelColonnes.Width div 3;
end;

procedure TFenRequetesLibres.LBOrderByDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:= (Source = LBProps) and (LBProps.ItemIndex >= 0);
end;

procedure TFenRequetesLibres.LBOrderByDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if (Source = LBProps) then
    LBOrderBy.AddField(TField(LBProps.Items.Objects[LBProps.ItemIndex]));
end;

procedure TFenRequetesLibres.btnEnleverColTriClick(Sender: TObject);
begin
  if LBOrderBy.ItemIndex >= 0 then
    LBOrderBy.Items.Delete(LBOrderBy.ItemIndex);
end;

procedure TFenRequetesLibres.LBPropsDblClick(Sender: TObject);
begin
  if LBProps.ItemIndex >= 0 then
    LBOrderBy.AddField(TField(LBProps.Items.Objects[LBProps.ItemIndex]));
end;

procedure TFenRequetesLibres.LBOrderByOrderChange(Sender: TObject;
  Ascending: Boolean);
begin
  if Ascending then
  begin
    if BtnChangeOrder.Tag <> 0 then
    begin
      BtnChangeOrder.Glyph.Assign(BitmapDescendingSort);
      BtnChangeOrder.Hint:= 'Trier la colonne en ordre Descendant';
      BtnChangeOrder.Tag:= 0;
    end;
  end else
  begin
    if BtnChangeOrder.Tag <> 1 then
    begin
      BtnChangeOrder.Glyph.Assign(BitmapAscendingSort);
      BtnChangeOrder.Hint:= 'Trier la colonne en ordre Ascendant';
      BtnChangeOrder.Tag:= 1;
    end;
  end;
end;

procedure TFenRequetesLibres.BtnChangeOrderClick(Sender: TObject);
begin
  LBOrderBy.ChangeSort;
end;

procedure TFenRequetesLibres.EditDisplayLabelKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    UpdateProps;
    Key:= #0;
  end;
end;

procedure TFenRequetesLibres.ChBVisibleClick(Sender: TObject);
begin
  UpDateProps;
end;

procedure TFenRequetesLibres.EditDisplayLabelExit(Sender: TObject);
begin
  UpDateProps;
end;

procedure TFenRequetesLibres.btnAppliquerClick(Sender: TObject);
begin
  FilterExpert.ApplySelection;
end;

procedure TFenRequetesLibres.GridResultatDblClick(Sender: TObject);
var
  k: Integer;
  APt: TPoint;
begin
  if (GetCursor = Screen.Cursors[crHSplit]) then
  begin
    GetCursorPos(APt);
    APt:= GridResultat.ScreenToClient(APt);
    k:= GridResultat.MouseCoord(APt.x - 7, APt.y).x - 1;
    GridResultat.SelectedIndex:= k;
    AutoDBGridColWidthIndex(GridResultat, k);
    Application.ProcessMessages;
    GridResultat.SelectedIndex:= k;
  end;
end;

{ LBProps }

procedure TFenRequetesLibres.LBPropsClick(Sender: TObject);
var
  AFi: TField;
begin
  if csLoading in ComponentState then
    Exit;
  {}
  if LBProps.ItemIndex >= 0 then
  begin
    AFi:= TField(LBProps.Items.Objects[LBProps.ItemIndex]);
    if (AFi <> nil) and (LBProps.Items[LBProps.ItemIndex] <> AFi.DisplayLabel) then
      LBProps.Items[LBProps.ItemIndex]:= AFi.DisplayLabel;
    SetProps(AFi);
  end else
    SetProps(nil);
end;

procedure TFenRequetesLibres.LBPropsClickCheck(Sender: TObject);
var
  AFi: TField;
begin
  if csLoading in ComponentState then
    Exit;
  {}
  if LBProps.ItemIndex >= 0 then
  begin
    AFi:= TField(LBProps.Items.Objects[LBProps.ItemIndex]);
    AFi.Visible:= LBProps.Checked[LBProps.ItemIndex];
    if (AFi <> nil) and (LBProps.Items[LBProps.ItemIndex] <> AFi.DisplayLabel) then
      LBProps.Items[LBProps.ItemIndex]:= AFi.DisplayLabel;
    SetProps(AFi);
  end else
    SetProps(nil);
end;

procedure TFenRequetesLibres.SetProps(AFi: TField);
begin
  if AFi <> nil then
  begin
    EditFieldName.Text:= AFi.FieldName;
    EditDisplayLabel.Text:= AFi.DisplayLabel;
    ChBVisible.Checked:= AFi.Visible;
    LBProps.Checked[LBProps.Items.IndexOfObject(AFI)]:= AFi.Visible;
  end else
  begin
    EditFieldName.Text:= '';
    EditDisplayLabel.Text:= '';
    ChBVisible.Checked:= False;
  end;
end;

procedure TFenRequetesLibres.UpDateProps;
var
  AFi: TField;
begin
  AFi:= DataSet.FindField(EditFieldName.Text);
  if AFi <> nil then
  begin
    if EditDisplayLabel.Text <> '' then
      AFi.DisplayLabel:= EditDisplayLabel.Text;
    AFi.Visible:= ChBVisible.Checked;
    LBProps.ItemIndex:= LBProps.Items.IndexOfObject(AFi);
    LBProps.OnClick(LBProps);
  end;
end;

procedure TFenRequetesLibres.RemplirProps;
var
  i: Integer;
  AFi: TField;
begin
  LBProps.Items.Clear;
  for i:= 0 to RequeteLibre.DefaultFormatCount - 1 do
  begin
    AFi:= DataSet.FindField(RequeteLibre.DefaultFormat[i].FieldName);
    LBProps.Items.AddObject(AFi.DisplayLabel, AFi);
    LBProps.Checked[i]:= AFi.Visible;
  end;
  LBProps.ItemIndex:= 0;
  LBProps.OnClick(LBProps);
end;

{}

procedure TFenRequetesLibres.NouvelleEdition;
var
  i: Integer;
  AFi: TField;
begin
  Mode:= mrQuery;
  DataSet.DisableControls;
  try
    FilterExpert.ClearSelection;
    BtnShowAll.OnClick(BtnShowAll);
    LBOrderBy.Clear;
    EditTitre.Text:= 'Nouvelle édition';
    RGOrientation.ItemIndex:= 0;
    GridResultat.Font.Assign(Font);
    GridResultat.TitleFont.Assign(Font);
    GridResultat.Columns.RestoreDefaults;
    for i:= 0 to RequeteLibre.DefaultFormatCount - 1 do
    begin
      AFi:= DataSet.FindField(RequeteLibre.DefaultFormat[i].FieldName);
      AFi.Index:= RequeteLibre.DefaultFormat[i].Index;
      AFi.DisplayLabel:= RequeteLibre.DefaultFormat[i].DisplayLabel;
      AFi.DisplayWidth:= RequeteLibre.DefaultFormat[i].DisplayWidth;
      AFi.Visible:= True;
    end;
    GridResultat.Columns.RestoreDefaults;
    RemplirProps;
    FilterExpert.ApplySelection;
  finally
    DataSet.EnableControls;
  end;
end;

{ Streaming }

type
  TFieldInfo = class(TComponent)
  private
    FFieldName: string;
    FDisplayWidth: Integer;
    FDisplayLabel: string;
    FVisible: Boolean;
    FIndex: Integer;
  published
    property FieldName: string read FFieldName write FFieldName;
    property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth;
    property DisplayLabel: string read FDisplayLabel write FDisplayLabel;
    property Index: Integer read FIndex write FIndex;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TColumnInfo = class(TComponent)
  private
    FWidth: Integer;
    FFieldName: string;
  published
    property FieldName: string read FFieldName write FFieldName;
    property Width: Integer read FWidth write FWidth;
  end;

  TInfoRequetesLibres = class(TComponent)
  private
    FOrientation: Integer;
    FReportTitle: string;
    FInfoFont: TFont;
    FTitleFont: TFont;
    {}
    procedure SetInfoFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property InfoFont: TFont read FInfoFont write SetInfoFont;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property ReportTitle: string read FReportTitle write FReportTitle;
    property ReportOrientation: Integer read FOrientation write FOrientation;
  end;

constructor TInfoRequetesLibres.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  {}
  FInfoFont:= TFont.Create;
  FTitleFont:= TFont.Create;
end;

destructor TInfoRequetesLibres.Destroy;
begin
  FInfoFont.Free;
  FTitleFont.Free;
  {}
  inherited Destroy;
end;

procedure TInfoRequetesLibres.SetInfoFont(const Value: TFont);
begin
  FInfoFont.Assign(Value);
end;

procedure TInfoRequetesLibres.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TFenRequetesLibres.SaveToStream(AStream: TStream);
  procedure SaveFieldInfo;
  var i: Integer;
      FI: TFieldInfo;
      F: TField;
  begin
    FI:= TFieldInfo.Create(nil);
    try
      i:= DataSet.FieldCount;
      AStream.Write(i, SizeOf(i));
      for i:= 0 to DataSet.FieldCount - 1 do
      begin
        F:= DataSet.Fields[i];
        FI.FieldName:= F.FieldName;
        FI.DisplayWidth:= F.DisplayWidth;
        FI.DisplayLabel:= F.DisplayLabel;
        FI.Index:= F.Index;
        FI.Visible:= F.Visible;
        AStream.WriteComponent(FI);
      end;
    finally
      FI.Free;
    end;
  end;
  procedure SaveGlobalInfo;
  var Info: TInfoRequetesLibres;
  begin
    Info:= TInfoRequetesLibres.Create(nil);
    try
      Info.InfoFont:= GridResultat.Font;
      Info.TitleFont:= GridResultat.TitleFont;
      Info.ReportTitle:= EditTitre.Text;
      Info.ReportOrientation:= RGOrientation.ItemIndex;
      AStream.WriteComponent(Info);
    finally
      Info.Free;
    end;
  end;
  procedure WriteVersion;
  begin
    TUsefulStream(AStream).WriteChar('G');
    TUsefulStream(AStream).WriteChar('Z');
    TUsefulStream(AStream).WriteChar('V');
    TUsefulStream(AStream).WriteLongInt(StreamVersion);
  end;
begin
  //GridResultat.Visible:= False; // Il faut la rendre invisible (pb au chargement)
  try
    WriteVersion;
    FilterExpert.SaveToStream(AStream); // Sauvegarde de la requête
    SaveFieldInfo; // Sauvegarde des champs et donc de la DBG
    LBOrderBy.SaveToStream(AStream); // Sauvegarde de la requête 2
    SaveGlobalInfo; // Sauvegarde des informations globales
    //V1
    SaveColumnInfo(AStream); //Sauvegarde des tailles des colonnes
  finally
    GridResultat.Visible:= True;
  end;
end;

procedure TFenRequetesLibres.LoadFromStream(AStream: TStream);
var
  V : Integer;
  procedure ReadFieldInfo;
  var i: Integer;
      FI: TFieldInfo;
      F: TField;
  begin
    AStream.Read(i, SizeOf(Integer));
    FI:= TFieldInfo.Create(nil);
    try
      for i:= 0 to i - 1 do
      begin
        AStream.ReadComponent(FI);
        F:= DataSet.FindField(FI.FieldName);
        if Assigned(F) then
        begin
          F.DisplayWidth:= FI.DisplayWidth;
          F.DisplayLabel:= FI.DisplayLabel;
          F.Index:= FI.Index;
          F.Visible:= FI.Visible;
        end;
      end;
    finally
      FI.Free;
    end;
  end;
  procedure ReadGlobalInfo;
  var Info: TInfoRequetesLibres;
  begin
    Info:= TInfoRequetesLibres.Create(nil);
    try
      AStream.ReadComponent(Info);
      GridResultat.Font:= Info.InfoFont;
      GridResultat.TitleFont:= Info.TitleFont;
      EditTitre.Text:= Info.ReportTitle;
      RGOrientation.ItemIndex:= Info.ReportOrientation;
    finally
      Info.Free;
    end;
  end;
  procedure CheckVersion;
  var
    OldPos : Int64;
  begin
    OldPos := AStream.Position;
    if (TUsefulStream(AStream).ReadChar = 'G') and (TUsefulStream(AStream).ReadChar = 'Z') and (TUsefulStream(AStream).ReadChar = 'V') then
      V := TUsefulStream(AStream).ReadLongInt
    else
    begin
      V := 0;
      AStream.Position := OldPos;
    end;
  end;
begin
  CheckVersion;
  FilterExpert.LoadFromStream(AStream);
  ReadFieldInfo;
  LBOrderBy.LoadFromStream(AStream, DataSet);
  ReadGlobalInfo;
  if V > 0 then
    ReadColumnInfo(AStream);

  //GridResultat.Visible:= True; // Rendue invisible pour cause de problèmes au chargement
  LBProps.Clear;
  RemplirProps;
end;

{ FilterExpert & SQL }

procedure TFenRequetesLibres.FilterExpertApply(Sender: TObject;
  Selection: string);
var
  OldTxt, AOrderBy: string;
  S: string;
  SQL: TStringList;
  AColStream : TStream;
begin
  SQL:= TStringList.Create;
  Screen.Cursor:= crHourGlass;
  try
    if Mode = mrQuery then
    begin
      AColStream := StoreColWidths;
      try
        OldTxt:= GetSQL(Query);
        try
          SQL.Assign(DefaultSQL);
          if Selection <> '' then
          begin
            if AnsiContainsText(SQL.Text, 'WHERE', ) then
              SQL.Add('AND')
            else
              SQL.Add('WHERE');
            SQL.Add(Selection);
          end;
          AOrderBy:= LBOrderBy.OrderBy;
          if (AOrderBy = '') and (DefaultOrderBy <> '') then
            SQL.Add(Format('ORDER BY %s', [DefaultOrderBy]))
          else if AOrderBy <> '' then
            SQL.Add(AOrderBy);
          if Assigned(RequeteLibre.BeforeExecSQL) then
          begin
            S:= SQL.Text;
            RequeteLibre.BeforeExecSQL(RequeteLibre, S);
            SQL.Text:= S;
          end;
          Query.Close;
          SetSQL(Query, SQL.Text);
          Query.Open;
        except
          if OldTxt <> '' then
          begin
            Query.Close;
            SetSQL(Query, OldTxt);
            Query.Open;
          end;
          raise;
        end;
      finally
        RestoreColWidths(AColStream);
      end;
    end else
    begin
      VDataSet.Filter:= Selection;
      VDataSet.IndexFieldNames:= LBOrderBy.IndexFieldNames;
    end;
    if RequeteLibre.AutoCount then
    begin
      FRecordCount:= GetRecordCount;
      PanelRecordCount.Caption:= AutomaticPlural(FRecordCount, 'enregistrement', 's');
    end else
    begin
      FRecordCount:= -1;
      PanelRecordCount.Caption:= '??? enregistrements';
    end;
  finally
    Screen.Cursor:= crDefault;
    SQL.Free;
  end;
end;

{ Impressions }

procedure TFenRequetesLibres.btnImpressionClick(Sender: TObject);
var
  DefaultPrint: Boolean;
  Coef: Double;
  procedure CreerColonnes(AGrid : TQRCustomGrid; LabelKind : TLabelKind);
  var
    ACG : TColumn;
    ACQ : TQRGridColumn;
    i : Integer;
    MW, CW, TW : Integer;
  begin
    MW := RepRequetesLibres.DetailBand1.Width;
    TW := 0;
    for i:= 0 to GridResultat.Columns.Count - 1 do
    begin
      ACG := GridResultat.Columns[i];
      ACQ := TQRGridColumn(AGrid.Columns.Add);
      ACQ.DataField := ACG.FieldName;
      ACQ.Font := AGrid.Font;
      ACQ.LabelKind := LabelKind;
      CW := Trunc(ACG.Width * Coef);
      if TW + CW < MW then
      begin
        ACQ.Width := CW;
        Inc(TW, CW);
      end
      else
      begin
        CW := MW - TW;
        ACQ.Width := CW;
        Exit;
      end;
    end;
  end;
begin
  //MessageDlg('Fonctionnalité désactivée', mtInformation, [mbOk], 0);

  DefaultPrint:= True;
  if Assigned(RequeteLibre.OnPrint) then
    RequeteLibre.OnPrint(RequeteLibre, DefaultPrint);
  if DefaultPrint then
  begin
    Screen.Cursor := crHourGlass;
    try
      RepRequetesLibres := TRepRequetesLibres.Create(Self);
      try
        RepRequetesLibres.PreviewInitialState := wsMaximized;
        
        RepRequetesLibres.ReportTitle := EditTitre.Text;
        if RGOrientation.ItemIndex = 0 then
          RepRequetesLibres.Page.Orientation := poPortrait
        else
          RepRequetesLibres.Page.Orientation := poLandscape;
        {}
        RepRequetesLibres.Font := GridResultat.Font;
        RepRequetesLibres.Font.Size := 12;
        RepRequetesLibres.Font.Style := [];
        RepRequetesLibres.QRTitre.Font := GridResultat.Font;
        RepRequetesLibres.QRTitre.Font.Size := 20;
        RepRequetesLibres.QRTitre.Font.Style := [fsBold];
        RepRequetesLibres.GridTitres.Font := GridResultat.TitleFont;
        RepRequetesLibres.GridDetail.Font := GridResultat.Font;

        RepRequetesLibres.DetailBand1.Height := Trunc(Abs(RepRequetesLibres.GridDetail.Font.Height) * 1.666) + 2;
        RepRequetesLibres.ColumnHeaderBand1.Height := Trunc(Abs(RepRequetesLibres.GridTitres.Font.Height) * 1.666) + 2;

        RepRequetesLibres.DataSet := Query;
        RepRequetesLibres.GridTitres.DataSet := Query;
        RepRequetesLibres.GridDetail.DataSet := Query;

        Coef := 1.25;//RepRequetesLibres.Printer.Canvas.TextWidth('AbCdE_') / GridResultat.Canvas.TextWidth('AbCdE_');

        CreerColonnes(RepRequetesLibres.GridTitres, lkFieldLabel);
        CreerColonnes(RepRequetesLibres.GridDetail, lkFieldValue);

        Query.DisableControls;
        try
          QRPreviewModal(RepRequetesLibres);
        finally
          Query.EnableControls;
          Query.First;
        end;
      finally
        RepRequetesLibres.Free;
      end;
    finally
      Screen.Cursor := crDefault;          
    end;
  end;
end;

  { Présentation }

procedure TFenRequetesLibres.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin
  if FEditFontTitle then
    GridResultat.TitleFont.Assign(FontDialog1.Font)
  else
    GridResultat.Font.Assign(FontDialog1.Font)
end;

procedure TFenRequetesLibres.BtnFontInfosClick(Sender: TObject);
var
  AFont : TFont;
begin
  AFont := TFont.Create;
  try
    FEditFontTitle := (TSpeedButton(Sender) = BtnFontTitres);
    if FEditFontTitle then
    begin
      FTitreFontDialog:= 'Police des Titres';
      AFont.Assign(GridResultat.TitleFont)
    end else
    begin
      FTitreFontDialog:= 'Police des Informations';
      AFont.Assign(GridResultat.Font);
    end;
    FontDialog1.Font.Assign(AFont);
    if FontDialog1.Execute then
    begin
      if FEditFontTitle then
        GridResultat.TitleFont.Assign(FontDialog1.Font)
      else
        GridResultat.Font.Assign(FontDialog1.Font);
    end else
    begin
      if FEditFontTitle then
        GridResultat.TitleFont.Assign(AFont)
      else
        GridResultat.Font.Assign(AFont);
    end;
  finally
    AFont.Free;
  end;
end;

procedure TFenRequetesLibres.FontDialog1Show(Sender: TObject);
begin
  SetWindowText(FontDialog1.Handle, PChar(FTitreFontDialog));
end;

{ Evénements sur la grille }

procedure TFenRequetesLibres.GridResultatColEnter(Sender: TObject);
begin
  if GridResultat.SelectedField <> nil then
  begin
    LBProps.ItemIndex:= LBProps.Items.IndexOfObject(GridResultat.SelectedField);
    LBProps.OnClick(LBProps);
  end;
end;

procedure TFenRequetesLibres.GridResultatTitleClick(Column: TColumn);
begin
  if Column.Field <> nil then
  begin
    LBProps.ItemIndex:= LBProps.Items.IndexOfObject(Column.Field);
    LBProps.OnClick(LBProps);
    GridResultat.SelectedIndex:= Column.Index;
  end;
end;

{ Evénements divers }

procedure TFenRequetesLibres.BtnShowAllClick(Sender: TObject);
var
  i: Integer;
  AFi: TField;
begin
  if LBProps.Items.Count > 0 then
  begin
    DataSet.DisableControls;
    try
      for i:= 0 to LBProps.Items.Count - 1 do
      begin
        AFi:= TField(LBProps.Items.Objects[i]);
        if Assigned(AFi) then
          AFi.Visible:= (Sender = BtnShowAll);
        LBProps.Checked[i]:= AFi.Visible;
      end;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

{ Exportation }

procedure TFenRequetesLibres.CSVExport(FileName: string; CSVFormat: TCSVFormat);
  procedure CSVSaveRecord(Stream: TStream);
  var i: Integer;
      F: TField;
      S, Temp: string;
  begin
    S:= '';
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + CSVFormat.FieldSeparator;
        Temp:= F.AsString;
        if (CSVFormat.QuoteChar <> '') and (DataSet.Fields[i].DataType in [ftString, ftMemo, ftDate, ftDateTime, ftTime]) then
          Temp:= AnsiQuotedStr(Temp, CSVFormat.QuoteChar[1]);
        Temp:= StringReplace(Temp, CSVFormat.LineBreak, ' ', [rfReplaceAll]);
        S:= S + Temp;
      end;
    end;
    S:= S + CSVFormat.LineBreak;
    Stream.Write(S[1], Length(S));
  end;
  procedure SaveHeader(Stream: TStream);
  var i: Integer;
      F: TField;
      S: string;
  begin
    S:= '';
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + CSVFormat.FieldSeparator;
        S:= S + F.DisplayLabel;
      end;
    end;
    S:= S + CSVFormat.LineBreak;
    Stream.Write(S[1], Length(S));
  end;
var F: TFileStream;
    Counter: Integer;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    Counter:= 0;
    SaveHeader(F);
    DataSet.First;
    while not DataSet.EOF do
    begin
      CSVSaveRecord(F);
      Inc(Counter);
      if Assigned(RequeteLibre.OnExportRecord) then
        RequeteLibre.OnExportRecord(RequeteLibre, Counter, FRecordCount);
      DataSet.Next;
    end;
    DataSet.First;
  finally
    F.Free;
  end;
end;

procedure TFenRequetesLibres.ConstExport(FileName: string);
  procedure TextConstSaveRecord(Stream: TStream);
  var i, j, L: Integer;
      F: TField;
      S, Temp: string;
  begin
    S:= '';
    Temp:= '';
    L:= 0;
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if F.Visible then
      begin
        case F.DataType of
          ftString: L:= F.Size;
          ftInteger, ftAutoInc: L:= 10; // Integer = Signed 32 bits => MaxVal = +/-2.10^9
          ftSmallint: L:= 6; // SmallInt = signed 16 bits   => MaxVal = +/- 32767
          ftWord: L:= 5;     // Word     = unsigned 16 bits => MaxVal = +   65535
          ftBoolean: L:= 1;  // Bool     = 0 or 1
          ftFloat, ftCurrency, ftBCD: L:= 21; // Arbitrary
          ftDate: L:= 10;    // Standard dd/mm/yyyy
          ftTime: L:= 8;     // Standard hh:nn:ss
          ftDateTime: L:= 19 // Standard dd/mm/yyyy hh:nn:ss
          else
            Continue;
        end;
        case F.DataType of
          ftBoolean:  Temp:= IntToStr(Abs(Integer(F.AsBoolean)));
          ftDate:     Temp:= FormatDateTime('dd/mm/yyyy', F.AsDateTime);
          ftTime:     Temp:= FormatDateTime('hh:nn:ss', F.AsDateTime);
          ftDateTime: Temp:= FormatDateTime('dd/mm/yyyy hh:nn:ss', F.AsDateTime);
          else
            Temp:= StringReplace(F.AsString, #13#10, ' ', [rfReplaceAll]);
        end;
        j:= Length(Temp);
        if j < L then
        begin // Fill with empty spaces
          SetLength(Temp, L);
          FillChar(Temp[j + 1], L - j, $20);
        end;
        S:= S + Temp;
      end;
    end;
    S:= S + #13#10;
    Stream.Write(S[1], Length(S));
  end;
var F: TFileStream;
    Counter: Integer;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    Counter:= 0;
    DataSet.First;
    while not DataSet.EOF do
    begin
      TextConstSaveRecord(F);
      Inc(Counter);
      if Assigned(RequeteLibre.OnExportRecord) then
        RequeteLibre.OnExportRecord(RequeteLibre, Counter, FRecordCount);
      DataSet.Next;
    end;
    DataSet.First;
  finally
    F.Free;
  end;
end;

procedure TFenRequetesLibres.btnExportClick(Sender: TObject);
begin
  FenExportDonnees:= TFenExportDonnees.Create(Self);
  try
    if FenExportDonnees.ShowModal = mrOk then
    begin
      DataSet.DisableControls;
      try
        if Assigned(RequeteLibre.BeforeExport) then
          RequeteLibre.BeforeExport(RequeteLibre);
        case FenExportDonnees.Format of
          fCSV: CSVExport(FenExportDonnees.FileName, FenExportDonnees.CSVFormat);
          fConstText: ConstExport(FenExportDonnees.FileName);
        end;
        if Assigned(RequeteLibre.AfterExport) then
          RequeteLibre.AfterExport(RequeteLibre);
      finally
        DataSet.EnableControls;
      end;
    end;
  finally
    FenExportDonnees.Free;
  end;
end;

{}

procedure TFenRequetesLibres.AutoAssignerDateModifModeles(DataSet: TDataSet);
var F: TField;
begin
  F:= DataSet.FieldByName(DataSetModeles.ChampDateCreation);
  if not F.IsNull then
    F:= DataSet.FieldByName(DataSetModeles.ChampDateModif);
  F.AsDateTime:= Now;
end;

procedure TFenRequetesLibres.AutoAssignerDateModifSnapShots(DataSet: TDataSet);
var F: TField;
begin
  F:= DataSet.FieldByName(DataSetSnapShots.ChampDateCreation);
  if not F.IsNull then
    F:= DataSet.FieldByName(DataSetSnapShots.ChampDateModif);
  F.AsDateTime:= Now;
end;

function TFenRequetesLibres.GetRecordCount: Integer;
var S: string;
    SQL: TStringList;
begin
  if Mode = mrQuery then
  begin
    if (RequeteLibre.DefaultCountSQL.Text = '') or (CountQuery = nil) then
    begin
      PanelRecordCount.Hide;
      Result:= 0;
      Exit;
    end;
    SQL:= TStringList.Create;
    try
      SQL.Assign(RequeteLibre.DefaultCountSQL);

      S:= FilterExpert.Selection;
      if S <> '' then
      begin
        if AnsiContainsText(SQL.Text, 'WHERE') then
          SQL.Add('AND ' + S)
        else
          SQL.Add('WHERE ' + S);
      end;
      {}
      if Assigned(RequeteLibre.BeforeExecSQL) then
      begin
        S:= SQL.Text;
        RequeteLibre.BeforeExecSQL(RequeteLibre, S);
        SQL.Text:= S;
      end;
      SetSQL(CountQuery, SQL.Text);
      CountQuery.Open;
      try
        Result:= CountQuery.Fields[0].AsInteger;
      finally
        CountQuery.Close;
      end;
    finally
      SQL.Free;
    end;
  end else
    Result:= VDataSet.RecordCount;
end;

function TFenRequetesLibres.GetDataSet: TDataSet;
begin
  if Mode = mrQuery then
    Result:= Query
  else
    Result:= VDataSet;
end;

type
  TPControl = class(TControl);

procedure TFenRequetesLibres.SetMode(const Value: TModeReqLibre);
begin
  FMode:= Value;
  case Value of
    mrQuery:
    begin
      Caption:= Format('%s : Travail sur les données réelles', [Title]);
      FilterExpert.Mode:= smSQL;
      GridResultat.Color:= clWindow;
    end;
    mrVDataSet:
    begin
      Caption:= Format('%s : Travail sur une image des données', [Title]);
      FilterExpert.Mode:= smFilter;
      GridResultat.Color:= $00C0DCC0;
    end;
  end;
  DSRequete.DataSet:= DataSet;
  FilterExpert.DataSet:= DataSet;
end;

{ Gestion des modèles }

procedure TFenRequetesLibres.btnInitEditionClick(Sender: TObject);
begin
  NouvelleEdition;
end;

procedure TFenRequetesLibres.btnChargerClick(Sender: TObject);
var
  AStream: TMemoryStream;
begin
  FenSelModeleEtat:= TFenSelModeleEtat.Create(nil);
  try
    FenSelModeleEtat.DataSetModeles:= DataSetModeles;
    FenSelModeleEtat.ModeEdition:= emModele;
    if FenSelModeleEtat.ShowModal = mrOk then
    begin
      Mode:= mrQuery;
      AStream:= TMemoryStream.Create;
      try
        (DataSetModeles.DataSet.FieldByName(DataSetModeles.ChampDonnees) as TBlobField).SaveToStream(AStream);
        AStream.Position:= 0;
        if AStream.Size > 0 then
        begin
          LoadFromStream(AStream);
          if MessageDlg('Voulez-vous appliquer la sélection maintenant ?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
              FilterExpert.ApplySelection;
        end
        else
          NouvelleEdition;
      finally
        AStream.Free;
      end;
    end;
  finally
    FenSelModeleEtat.Free;
  end;
end;

procedure TFenRequetesLibres.btnEnregistrerClick(Sender: TObject);
var AStream: TMemoryStream;
begin
  DataSetModeles.DataSet.Open;
  FenEditModeleEtat:= TFenEditModeleEtat.Create(nil);
  try
    FenEditModeleEtat.DataSetModeles:= DataSetModeles;
    FenEditModeleEtat.ModeEdition:= emModele;
    {}
    FenEditModeleEtat.EditNom.Text:= EditTitre.Text;
    {}
    if FenEditModeleEtat.ShowModal = mrOk then
    begin
      EditTitre.Text:= FenEditModeleEtat.EditNom.Text;
      AStream:= TMemoryStream.Create;
      try
        AStream.Position:= 0;
        SaveToStream(AStream);
        {}
        DataSetModeles.DataSet.FieldByName(DataSetModeles.ChampNom).AsString:= FenEditModeleEtat.EditNom.Text;
        DataSetModeles.DataSet.FieldByName(DataSetModeles.ChampCommentaires).AsString:=
          FenEditModeleEtat.MemoCommentaires.Lines.Text;
        (DataSetModeles.DataSet.FieldByName(DataSetModeles.ChampDonnees) as TBlobField).LoadFromStream(AStream);
        DataSetModeles.DataSet.Post;
      finally
        AStream.Free;
      end;
    end;
  finally
    FenEditModeleEtat.Free;
  end;
end;

procedure TFenRequetesLibres.btnEditerModelesClick(Sender: TObject);
begin
  DataSetModeles.DataSet.Open;
  if DataSetModeles.DataSet.RecordCount = 0 then
  begin
    DataSetModeles.DataSet.Close;
    Exit;
  end;
  FenEditModelesEtats:= TFenEditModelesEtats.Create(Self);
  try
    FenEditModelesEtats.DataSetModeles:= DataSetModeles;
    FenEditModelesEtats.ModeEdition:= emModele;
    FenEditModelesEtats.ShowModal;
  finally
    FenEditModelesEtats.Free;
  end;
end;

{ Gestion des SnapShots }

procedure TFenRequetesLibres.btnSaveSnapShotClick(Sender: TObject);
var AStream: TMemoryStream;
    S: TDataSetStreamer;
begin
  DataSetSnapShots.DataSet.Open;
  FenEditModeleEtat:= TFenEditModeleEtat.Create(nil);
  try
    FenEditModeleEtat.DataSetModeles:= DataSetSnapShots;
    FenEditModeleEtat.ModeEdition:= emSnapShot;
    {}
    FenEditModeleEtat.EditNom.Text:= EditTitre.Text;
    {}
    if FenEditModeleEtat.ShowModal = mrOk then
    begin
      EditTitre.Text:= FenEditModeleEtat.EditNom.Text;
      S:= TDataSetStreamer.Create(DataSet, efDataSet, False);
      AStream:= TMemoryStream.Create;
      try
        AStream.Position:= 0;
        S.SaveToStream(AStream); // Sauvegarde des données
        SaveToStream(AStream); // Sauvegarde de la mise en forme
        {}
        DataSetSnapShots.DataSet.FieldByName(DataSetSnapShots.ChampNom).AsString:= FenEditModeleEtat.EditNom.Text;
        DataSetSnapShots.DataSet.FieldByName(DataSetSnapShots.ChampCommentaires).AsString:=
          FenEditModeleEtat.MemoCommentaires.Lines.Text;
        (DataSetSnapShots.DataSet.FieldByName(DataSetSnapShots.ChampDonnees) as TBlobField).LoadFromStream(AStream);
        DataSetSnapShots.DataSet.Post;
      finally
        AStream.Free;
        S.Free;
      end;
    end;
  finally
    FenEditModeleEtat.Free;
  end;
end;

procedure TFenRequetesLibres.btnLoadSnapShotClick(Sender: TObject);
var
  AStream: TMemoryStream;
begin
  FenSelModeleEtat:= TFenSelModeleEtat.Create(nil);
  try
    FenSelModeleEtat.DataSetModeles:= DataSetSnapShots;
    FenSelModeleEtat.ModeEdition:= emSnapShot;
    if FenSelModeleEtat.ShowModal = mrOk then
    begin
      Mode:= mrVDataSet;
      AStream:= TMemoryStream.Create;
      try
        (DataSetSnapShots.DataSet.FieldByName(DataSetSnapShots.ChampDonnees) as TBlobField).SaveToStream(AStream);
        AStream.Position:= 0;
        if AStream.Size > 0 then
        begin
          VDataSet.LoadFromStream(AStream); // Charger les données
          LoadFromStream(AStream); // Charger la mise en forme
          FilterExpert.UserSelection.Clear;
          FilterExpert.ApplySelection;
          Caption:= Format('%s : Travail sur une l''image [%s]',
            [Title, DataSetSnapShots.DataSet.FieldByName(DataSetSnapShots.ChampNom).AsString]);
        end else
          NouvelleEdition;
      finally
        AStream.Free;
      end;
    end;
  finally
    FenSelModeleEtat.Free;
  end;
end;

procedure TFenRequetesLibres.btnEditSnapShotsClick(Sender: TObject);
begin
  DataSetSnapShots.DataSet.Open;
  if DataSetSnapShots.DataSet.RecordCount = 0 then
  begin
    DataSetSnapShots.DataSet.Close;
    Exit;
  end;
  FenEditModelesEtats:= TFenEditModelesEtats.Create(Self);
  try
    FenEditModelesEtats.DataSetModeles:= DataSetSnapShots;
    FenEditModelesEtats.ModeEdition:= emSnapShot;
    FenEditModelesEtats.ShowModal;
  finally
    FenEditModelesEtats.Free;
  end;
end;

procedure TFenRequetesLibres.FilterExpertEditWithButtonClick(Sender: TObject;
  FieldName: String; var Value, InternalValue: String);
begin
  if Assigned(RequeteLibre.OnFilterExpertEditButtonClick) then
    RequeteLibre.OnFilterExpertEditButtonClick(Sender, FieldName, Value, InternalValue);
end;

procedure TFenRequetesLibres.FilterExpertFillValues(Sender: TObject;
  FieldName: String; List: TStrings);
begin
  if Assigned(RequeteLibre.OnFilterExpertFillValues) then
    RequeteLibre.OnFilterExpertFillValues(Sender, FieldName, List);
end;

procedure TFenRequetesLibres.FilterExpertSelectField(Sender: TObject;
  FieldName: String; var EditKind: TEditKind);
begin
  if Assigned(RequeteLibre.OnFilterExpertSelectField) then
    RequeteLibre.OnFilterExpertSelectField(Sender, FieldName, EditKind);
end;

procedure TFenRequetesLibres.WMSize(var Message: TWMSize);
  procedure SetAnimation(Value: Boolean);
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    BOOL(Info.iMinAnimate) := Value;
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
  end;
begin
  if Message.SizeType = SIZE_MINIMIZED then
  begin
    SetAnimation(False);
    ShowWindow(Application.Handle, SW_MINIMIZE);
  end
  else
    // CB : 21/05/02
    // Si le message n'est pas celui qu'on traite il faut qd meme appeler
    // le gestionnaire d'évenement par defaut
    inherited;
end;

procedure TFenRequetesLibres.ExitPanelOkClick(Sender: TObject);
begin
  if Assigned(FReqLibre.OnOkClick) then
    FReqLibre.OnOkClick(Sender, FilterExpert.Selection);
  Close;
end;

procedure TFenRequetesLibres.ExitPanelCancelClick(Sender: TObject);
begin
  if Assigned(FReqLibre.OnCancelClick) then
    FReqLibre.OnCancelClick(Sender);
  Close;
end;

procedure TFenRequetesLibres.PanelRecordCountDblClick(Sender: TObject);
begin
  if FRecordCount = -1 then
  begin
    FRecordCount:= GetRecordCount;
    PanelRecordCount.Caption:= AutomaticPlural(FRecordCount, 'enregistrement', 's');
  end;
end;

function TFenRequetesLibres.GetSQL(const DataSet: TDataSet): string;
var P: PPropInfo;
    S: TStrings;
begin
  P:= GetPropInfo(DataSet, 'SQL');
  if P <> nil then
  begin
    S:= Pointer(GetOrdProp(DataSet, P));
    Result:= S.Text;
  end
  else
  begin
    P:= GetPropInfo(DataSet, 'CommandText');
    if P <> nil then
      Result:= GetStrProp(DataSet, 'CommandText')
    else
    begin
      P:= GetPropInfo(DataSet, 'SQLSelect');
      if P <> nil then
      begin
        S:= Pointer(GetOrdProp(DataSet, P));
        Result:= S.Text;
      end
      else
        raise Exception.CreateFmt('Classe de DataSet %s non supportée', [DataSet.ClassName]);
    end;
  end;
end;

procedure TFenRequetesLibres.SetSQL(const DataSet: TDataSet; const SQL: string);
var P: PPropInfo;
    S: TStrings;
begin
  P:= GetPropInfo(DataSet, 'SQL');
  if P <> nil then
  begin
    S:= Pointer(GetOrdProp(DataSet, P));
    S.Text:= SQL;
  end
  else
  begin
    P:= GetPropInfo(DataSet, 'CommandText');
    if P <> nil then
      SetStrProp(DataSet, 'CommandText', SQL)
    else
    begin
      P:= GetPropInfo(DataSet, 'SQLSelect');
      if P <> nil then
      begin
        S:= Pointer(GetOrdProp(DataSet, P));
        S.Text:= SQL;
      end
      else
        raise Exception.CreateFmt('%s non supporté', [DataSet.ClassName]);
    end;
  end;
  DataSet.Close;
end;

procedure TFenRequetesLibres.FormHide(Sender: TObject);
begin
  if Assigned(RequeteLibre.OnShow) then
    RequeteLibre.OnShow(Sender);
end;

procedure TFenRequetesLibres.btnCopierGrilleClick(Sender: TObject);
begin
  DataSet.DisableControls;
  try
    CopyExport;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TFenRequetesLibres.CopyExport;
  procedure SaveRecord(Stream: TStream);
  var i: Integer;
      F: TField;
      S, Temp: string;
  begin
    S:= '';
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + #9;
        Temp:= F.AsString;
        if (DataSet.Fields[i].DataType in [ftString, ftMemo, ftDate, ftDateTime, ftTime]) then
          Temp:= AnsiQuotedStr(Temp, '"');
        Temp:= StringReplace(Temp, #13#10, ' ', [rfReplaceAll]);
        Temp:= StringReplace(Temp, #13, ' ', [rfReplaceAll]);
        Temp:= StringReplace(Temp, #10, ' ', [rfReplaceAll]);
        S:= S + Temp;
      end;
    end;
    S:= S + #13#10;
    Stream.Write(S[1], Length(S));
  end;
  procedure SaveHeader(Stream: TStream);
  var i: Integer;
      F: TField;
      S: string;
  begin
    S:= '';
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + #9;
        S:= S + F.DisplayLabel;
      end;
    end;
    S:= S + #13#10;
    Stream.Write(S[1], Length(S));
  end;
var F: TMemoryStream;
    //Counter: Integer;
    k: Byte;
begin
  F:= TMemoryStream.Create;
  try
    //Counter:= 0;
    SaveHeader(F);
    DataSet.First;
    while not DataSet.EOF do
    begin
      SaveRecord(F);
      //Inc(Counter);
      DataSet.Next;
    end;
    k := 0;
    F.Write(k, 1);
    Clipboard.SetTextBuf(F.Memory);
    DataSet.First;
  finally
    F.Free;
  end;
end;

procedure TFenRequetesLibres.SaveColumnInfo(AStream: TStream);
var
  i: Integer;
  CI: TColumnInfo;
  C: TColumn;
begin
  CI:= TColumnInfo.Create(nil);
  try
    i:= GridResultat.Columns.Count;
    AStream.Write(i, SizeOf(i));
    for i:= 0 to GridResultat.Columns.Count - 1 do
    begin
      C:= GridResultat.Columns[i];
      CI.FieldName:= C.FieldName;
      if C.Width <> C.DefaultWidth then
        CI.Width:= C.Width;
      AStream.WriteComponent(CI);
    end;
  finally
    CI.Free;
  end;
end;

procedure TFenRequetesLibres.ReadColumnInfo(AStream: TStream);
var
  i: Integer;
  CI: TColumnInfo;
  C: TColumn;
  function FindColumn : TColumn;
  var
    k : Integer;
  begin
    Result := nil;
    for k := 0 to GridResultat.Columns.Count - 1 do
      if GridResultat.Columns[k].FieldName = CI.FieldName then
      begin
        Result := GridResultat.Columns[k];
        Exit;
      end;
  end;
begin
  AStream.Read(i, SizeOf(Integer));
  CI:= TColumnInfo.Create(nil);
  try
    for i:= 0 to i - 1 do
    begin
      AStream.ReadComponent(CI);
      C:= FindColumn;
      if Assigned(C) and (CI.Width > 0) and (CI.Width < 500) then
      begin
        C.Width := CI.Width;
      end;
    end;
  finally
    CI.Free;
  end;
end;

procedure TFenRequetesLibres.RestoreColWidths(AStream: TStream);
begin
  try
    ReadColumnInfo(AStream);
  finally
    AStream.Free;
  end;
end;

function TFenRequetesLibres.StoreColWidths: TStream;
begin
  Result := TMemoryStream.Create;
  SaveColumnInfo(Result);
  Result.Position := 0;
end;

end.

