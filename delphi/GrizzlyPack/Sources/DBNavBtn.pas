unit DBNavBtn;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, DB, Buttons;

type
  TDBNavBarre = class;

  TInfoDataLink = class(TDataLink)
  private
    FDBNavBarre: TDBNavBarre;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ADBNavB: TDBNavBarre);
    destructor Destroy; override;
  end;

  TNavMode = (nmSingleDatasource, nmMultiDatasource);

  TNavButtons = (navInsert, navEdit, navDelete, navPost, navCancel);
  TNavButtonsSet = set of TNavButtons;

  TNavBarreAfterEvent = procedure(Sender: TObject; Button: TNavButtons) of object;
  TNavBarreBeforeEvent = procedure(Sender: TObject; Button: TNavButtons; var AllowAction: Boolean) of object;

  TNavInsertMode = (imInsert, imInsertFirst, imInsertLast);

  TDBNavBarre = class(TCustomPanel)
  private
    { Déclarations private }
    FBeforeAction: TNavBarreBeforeEvent;
    FAfterAction: TNavBarreAfterEvent;
    FDataLink: TInfoDataLink;
    FDataLinks: TList;
    FVisibleBtn: TNavButtonsSet;
    FNbBtn: Byte;
    FFocusControl: TWinControl;
    AjouterBtn, ModifierBtn, SauverBtn, AnnulerBtn, EffacerBtn: TBitBtn;
    FLabels, FHints: TStringList;
    FDeleteConfirmation: Boolean;
    FDeleteMessage: string;
    FNavInsertMode: TNavInsertMode;
    FInversionMode: Boolean;
    procedure BougerBoutons;
    function GetLabels: TStrings;
    procedure SetLabels(Labels: TStrings);
    function GetHints: TStrings;
    procedure SetHints(Hints: TStrings);
    function GetDeleteMessage: string;
    procedure LabelsChange(Sender: TObject);
    procedure HintsChange(Sender: TObject);
    procedure SetCancelEnabled(const Value: Boolean);
    procedure SetDefaultEnabled(const Value: Boolean);
    function GetCancelEnabled: Boolean;
    function GetDefaultEnabled: Boolean;
    procedure SetNavInsertMonde(const Value: TNavInsertMode);
    procedure SetInversionMode(const Value: Boolean);
  protected
    { Déclarations protected }
    Liste2Btn: TList;
    FMarginWidth: Integer;
    FMarginHeight: Integer;
    FSpacing: Integer;
    FBtnHeight: Integer;
    FBtnWidth: Integer;
    FNavMode: TNavMode;
    FDSWaiting: TList;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetFocusControl(Value: TWinControl);
    procedure SetSpacing(Value: Integer);
    procedure SetMarginHeight(Value: Integer);
    procedure SetMarginWidth(Value: Integer);
    procedure CreerBoutons;
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource;
    procedure SetBtnSpacing(Value: Integer);
    function GetBtnSpacing: Integer;
    procedure SetBtnMargin(Value: Integer);
    function GetBtnMargin: Integer;
    procedure SetBtnWidth(Value: Integer);
    procedure SetBtnHeight(Value: Integer);
    procedure SetVisibleBtn(Value: TNavButtonsSet);
    procedure EditingChanged;
    procedure DataChanged;
    procedure ActiveChanged;
    procedure Paint; override;
    procedure AjouterBtnClick(Sender: TObject);
    procedure ModifierBtnClick(Sender: TObject);
    procedure SauverBtnClick(Sender: TObject);
    procedure AnnulerBtnClick(Sender: TObject);
    procedure EffacerBtnClick(Sender: TObject);
    procedure Resize; override;
    procedure SetNavMode(Value: TNavMode);
    function CreateDatalink(UnDS: TDatasource): TInfoDataLink;
    function GetDataLinks: TList;
    property DataLinks: TList read GetDataLinks;
    procedure DoBeforeAction(ABtn: TNavButtons; var DoAction: Boolean);
    procedure DoAfterAction(ABtn: TNavButtons);
  public
    { Déclarations public }
    procedure SetFocus; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Editing: Boolean;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BtnPost: TBitBtn read SauverBtn;
    property BtnCancel: TBitBtn read AnnulerBtn;
    property BtnInsert: TBitBtn read AjouterBtn;
    property BtnUpdate: TBitBtn read ModifierBtn;
    property BtnDelete: TBitBtn read EffacerBtn;
  published
    { Déclarations published }
    property Align;
    property Anchors;
    property BorderWidth;
    property BevelWidth;
    property BevelInner;
    property BevelOuter;
    property Font;
    property Color;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TabStop;
    property Visible;
    property Enabled;
    property NavMode: TNavMode read FNavMode write SetNavMode;
    property InsertMode: TNavInsertMode read FNavInsertMode write SetNavInsertMonde default imInsert;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property BtnSpacing: Integer read GetBtnSpacing write SetBtnSpacing default 4;
    property BtnMargin: Integer read GetBtnMargin write SetBtnMargin default -1;
    property BtnWidth: Integer read FBtnWidth write SetBtnWidth default -1;
    property BtnHeight: Integer read FBtnHeight write SetBtnHeight default -1;
    property Spacing: Integer read FSpacing write SetSpacing default -1;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight default -1;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth default -1;
    property VisibleBtn: TNavButtonsSet read FVisibleBtn write SetVisibleBtn default [navInsert, navEdit, navPost, navCancel];
    property BtnLabels: TStrings read GetLabels write SetLabels;
    property BtnHints: TStrings read GetHints write SetHints;
    property DeleteConfirmation: Boolean read FDeleteConfirmation write FDeleteConfirmation default True;
    property DeleteMessage: string read GetDeleteMessage write FDeleteMessage;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property BeforeAction: TNavBarreBeforeEvent read FBeforeAction write FBeforeAction;
    property AfterAction: TNavBarreAfterEvent read FAfterAction write FAfterAction;
    property DefaultEnabled : Boolean read GetDefaultEnabled write SetDefaultEnabled default True;
    property CancelEnabled : Boolean read GetCancelEnabled write SetCancelEnabled default True;
    property InversionMode : Boolean read FInversionMode write SetInversionMode default False;
  end;

implementation

uses DBConsts, UFGDBCtl, DBNavMgr;

{$R DBNAVBTN32}

procedure TDBNavBarre.SetVisibleBtn(Value: TNavButtonsSet);
begin
  if Value <> [] then
  begin
    FVisibleBtn:= Value;
    BougerBoutons;
  end;
end;

function TDBNavBarre.GetDataLinks: TList;
var
  i: Integer;
begin
  Result:= FDataLinks;
  if (FDataLinks <> nil) and (FDSWaiting <> nil) and (FDSWaiting.Count > 0) then
  begin
    for i:= 0 to FDSWaiting.Count - 1 do
      FDataLinks.Add(CreateDataLink(TDataSource(FDSWaiting.Items[i])));
    FDSWaiting.Clear;
  end;
end;

function TDBNavBarre.Editing: Boolean;
  function GetMultiEditing: Boolean;
  var
    i: Integer;
  begin
    Result:= False;
    if (DataLinks <> nil) and (DataLinks.Count > 0) then
    begin
      Result:= False;
      for i:= 0 to DataLinks.Count - 1 do
      begin
        if TInfoDataLink(DataLinks.Items[i]).Editing then
        begin
          Result:= True;
          Exit;
        end;
      end;
    end;
  end;
begin
  case NavMode of
    nmSingleDataSource: Result:= FDatalink.Editing;
    nmMultiDatasource: Result:= GetMultiEditing;
  else
    Result:= False;
  end;
end;

function TDBNavBarre.CreateDatalink(UnDS: TDatasource): TInfoDataLink;
begin
  try
    Result:= TInfoDataLink.Create(Self);
    Result.DataSource:= UnDS;
  except
    raise;
  end;
end;

procedure TDBNavBarre.SetNavMode(Value: TNavMode);
  procedure SetSingle;
  var
    i: Integer;
  begin
    if FDataLinks <> nil then
    begin
      for i:= 0 to FDataLinks.Count - 1 do
      begin
        TObject(FDataLinks.Items[i]).Free;
      end;
      FDataLinks.Free;
      FDataLinks:= nil;
    end;
    FNavMode:= nmSingleDatasource;
  end;
  procedure SetMulti;
  var
    i: Integer;
    Origine: TComponent;
  begin
    if FDataLinks = nil then
      FDataLinks:= TList.Create;
    Origine:= Owner;
    for i:= 0 to Origine.ComponentCount - 1 do
    begin
      if Origine.Components[i] is TDatasource then
        FDataLinks.Add(CreateDatalink(Origine.Components[i] as TDatasource));
    end;
    if FDSWaiting <> nil then
      FDSWaiting.Clear;
    FNavMode:= nmMultiDatasource;
  end;
begin
  if Value <> FNavMode then
  begin
    case Value of
      nmSingleDatasource:
        SetSingle;
      nmMultiDatasource:
        SetMulti;
    end;
  end;
end;

procedure TDBNavBarre.Notification(AComponent: TComponent; Operation: TOperation);
var
  LeDL: TInfoDataLink;
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;
  if AComponent is TDataSource then
  begin
    if Operation = opRemove then
    begin
      if TDataSource(AComponent) = DataSource then
        Datasource:= nil;
      if (FDataLinks <> nil) and (FDataLinks.Count > 0) then
      begin
        for i:= 0 to FDatalinks.Count - 1 do
        begin
          if TInfoDataLink(FDataLinks.Items[i]).DataSource = TDataSource(AComponent) then
          begin
            LeDL:= TInfoDataLink(FDataLinks.Items[i]);
            FDatalinks.Remove(LeDL);
            LeDL.Free;
          end;
        end;
        if FDSWaiting.IndexOf(AComponent) >= 0 then
          FDSWaiting.Remove(AComponent);
      end;
    end
    else
    begin
      if (FDSWaiting <> nil) then
      begin
        if (FDatalinks <> nil) and (FDataLinks.Count > 0) then
        begin
          for i:= 0 to FDatalinks.Count - 1 do
          begin
            if TInfoDataLink(FDataLinks.Items[i]).DataSource = TDataSource(AComponent) then
            begin
              Exit;
            end;
          end;
        end;
        if FDSWaiting.IndexOf(AComponent) < 0 then
          FDSWaiting.Add(TDataSource(AComponent));
      end;
    end;
  end
  else if AComponent is TWinControl then
  begin
    if (Operation = opRemove) and (AComponent = FFocusControl) then
    begin
      SetFocusControl(nil);
    end;
  end;
end;

procedure TDBNavBarre.SetBtnWidth(Value: Integer);
begin
  if Value <> BtnWidth then
  begin
    if Value < 0 then
      Value:= -1;
    FBtnWidth:= Value;
    BougerBoutons;
  end;
end;

procedure TDBNavBarre.SetBtnHeight(Value: Integer);
begin
  if Value <> BtnHeight then
  begin
    if Value < 0 then
      Value:= -1;
    FBtnHeight:= Value;
    BougerBoutons;
  end;
end;

procedure TDBNavBarre.SetBtnSpacing(Value: Integer);
var
  i: Integer;
begin
  if Value < -1 then
    Value:= -1;
  for i:= 0 to Liste2Btn.Count - 1 do
    TBitBtn(Liste2Btn.Items[i]).Spacing:= Value;
end;

function TDBNavBarre.GetBtnSpacing: Integer;
begin
  Result:= TBitBtn(Liste2Btn.Items[0]).Spacing;
end;

procedure TDBNavBarre.SetBtnMargin(Value: Integer);
var
  i: Integer;
begin
  if Value < -1 then
    Value:= -1;
  for i:= 0 to Liste2Btn.Count - 1 do
    TBitBtn(Liste2Btn.Items[i]).Margin:= Value;
end;

function TDBNavBarre.GetBtnMargin: Integer;
begin
  Result:= TBitBtn(Liste2Btn.Items[0]).Margin;
end;

procedure TDBNavBarre.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    if Value < 0 then
      FSpacing:= -1
    else
      FSpacing:= Value;
    BougerBoutons;
  end;
end;

procedure TDBNavBarre.SetMarginHeight(Value: Integer);
begin
  if Value <> FMarginHeight then
  begin
    if Value < 0 then
      FMarginHeight:= -1
    else
      FMarginHeight:= Value;
    BougerBoutons;
  end;
end;

procedure TDBNavBarre.SetMarginWidth(Value: Integer);
begin
  if Value <> FMarginWidth then
  begin
    if Value < 0 then
      FMarginWidth:= -1
    else
      FMarginWidth:= Value;
    BougerBoutons;
  end;
end;

procedure TDBNavBarre.BougerBoutons;
var
  Gauche, Espace, Haut: Integer;
  Largeur, Hauteur: Integer;
  TailleBord: Integer;
  HautTotal, LargTotal: Integer;
  i: Integer;
  LeBtn, LeDernier: TBitBtn;
  IsEditing : Boolean;
  function EditVisible : Boolean;
  begin
    if FInversionMode then
      Result := not IsEditing
    else
      Result := True;
  end;
  function SaveVisible : Boolean;
  begin
    if FInversionMode then
      Result := IsEditing
    else
      Result := True;
  end;
begin
  IsEditing := Enabled and Assigned(FDataLink) and Assigned(FDataLink.Dataset) and FDataLink.Active and FDataLink.DataSet.CanModify and FDataLink.Editing;
  AjouterBtn.Visible:= (navInsert in VisibleBtn) and EditVisible;
  ModifierBtn.Visible:= (navEdit in VisibleBtn) and EditVisible;
  SauverBtn.Visible:= (navPost in VisibleBtn) and SaveVisible;
  AnnulerBtn.Visible:= (navCancel in VisibleBtn) and SaveVisible;
  EffacerBtn.Visible:= (navDelete in VisibleBtn) and EditVisible;
  FNbBtn:= 0;
  //Le -100 est nécessaire pour que le bouton soit bien invisible en design...
  if AjouterBtn.Visible then
    Inc(FNbBtn)
  else
    AjouterBtn.Top:= -100;
  if ModifierBtn.Visible then
    Inc(FNbBtn)
  else
    ModifierBtn.Top:= -100;
  if SauverBtn.Visible then
    Inc(FNbBtn)
  else
    SauverBtn.Top:= -100;
  if AnnulerBtn.Visible then
    Inc(FNbBtn)
  else
    AnnulerBtn.Top:= -100;
  if EffacerBtn.Visible then
    Inc(FNbBtn)
  else
    EffacerBtn.Top:= -100;

  TailleBord:= 0;
  LargTotal:= Width - TailleBord * 2;
  HautTotal:= Height - TailleBord * 2;
  if BtnWidth = -1 then
  begin
    if (Spacing = -1) and (MarginWidth = -1) then
      Largeur:= LargTotal div FNbBtn
    else if Spacing = -1 then
      Largeur:= (LargTotal - MarginWidth * 2) div FNbBtn
    else if MarginWidth = -1 then
      Largeur:= (LargTotal - Spacing * (FNbBtn - 1)) div FNbBtn
    else
      Largeur:= (LargTotal - Spacing * (FNbBtn - 1) - MarginWidth * 2) div FNbBtn;
  end
  else
    Largeur:= BtnWidth;
  if BtnHeight = -1 then
  begin
    if MarginHeight = -1 then
      Hauteur:= HautTotal
    else
      Hauteur:= (HautTotal - MarginHeight * 2);
  end
  else
    Hauteur:= BtnHeight;
  if (Spacing = -1) and (MarginWidth = -1) then
  begin
    Gauche:= (LargTotal - Largeur * FNbBtn) div (FNbBtn + 1);
    Espace:= Gauche;
  end
  else if (MarginWidth = -1) then
  begin
    Espace:= Spacing;
    Gauche:= (LargTotal - Largeur * FNbBtn - Espace * (FNbBtn - 1)) div 2;
  end
  else if (Spacing = -1) then
  begin
    Gauche:= MarginWidth;
    Espace:= (LargTotal - Largeur * FNbBtn - Gauche * 2) div (FNbBtn - 1);
  end
  else
  begin
    Gauche:= MarginWidth;
    Espace:= Spacing;
  end;
  if MarginHeight = -1 then
    Haut:= (HautTotal - Hauteur) div 2
  else
    Haut:= MarginHeight;
  LeDernier:= nil;
  for i:= 0 to Liste2Btn.Count - 1 do
  begin
    LeBtn:= TBitBtn(Liste2Btn.Items[i]);
    if LeBtn.Visible then
    begin
      if LeDernier = nil then
        LeBtn.SetBounds(Gauche, Haut, Largeur, Hauteur)
      else
        LeBtn.SetBounds(LeDernier.Left + LeDernier.Width + Espace, Haut, Largeur, Hauteur);
      LeDernier:= LeBtn;
    end;
  end;
  if (LeDernier <> nil) and (LargTotal - LeDernier.Left - LeDernier.Width > Gauche) then
    LeDernier.Width:= LargTotal - LeDernier.Left - Gauche;
end;

procedure TDBNavBarre.Resize;
begin
  inherited Resize;
  BougerBoutons;
end;

procedure TDBNavBarre.SetFocusControl(Value: TWinControl);
begin
  FFocusControl:= Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TDBNavBarre.SetDataSource(Value: TDataSource);
begin
  if Assigned(FDataLink) then
    FDataLink.DataSource:= Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TDBNavBarre.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result:= FDataLink.DataSource
  else
    Result:= nil;
end;

procedure TDBNavBarre.DoBeforeAction(ABtn: TNavButtons; var DoAction: Boolean);
begin
  DoAction:= True;
  if Assigned(FBeforeAction) then
    FBeforeAction(Self, ABtn, DoAction);
end;

procedure TDBNavBarre.DoAfterAction(ABtn: TNavButtons);
begin
  if (ABtn in [navInsert, navEdit]) and (FocusControl <> nil) then
    FocusControl.SetFocus;
  if Assigned(FAfterAction) then
    FAfterAction(Self, ABtn);
end;

procedure TDBNavBarre.AjouterBtnClick(Sender: TObject);
var DoAction: Boolean;
begin
  if (FDataLink.Datasource <> nil) and (Datasource.Dataset <> nil) and
    not (Datasource.State in [dsEdit, dsInsert]) then
  begin
    DoBeforeAction(navInsert, DoAction);
    if DoAction then
    begin
      case FNavInsertMode of
      imInsert : Datasource.Dataset.Insert;
      imInsertFirst :
        begin
          Datasource.Dataset.First;
          Datasource.Dataset.Insert;
        end;
      imInsertLast :
        begin
          Datasource.Dataset.Append;
        end;
      end;
      DoAfterAction(navInsert);
    end;
  end;
end;

procedure TDBNavBarre.ModifierBtnClick(Sender: TObject);
var DoAction: Boolean;
begin
  if (FDataLink.Datasource <> nil) and
    (Datasource.Dataset <> nil) and
    not (Datasource.State in [dsEdit, dsInsert]) then
  begin
    DoBeforeAction(navEdit, DoAction);
    if DoAction then
    begin
      Datasource.Dataset.Edit;
      DoAfterAction(navEdit);
    end;
  end;
end;

procedure TDBNavBarre.EffacerBtnClick(Sender: TObject);
var DoAction: Boolean;
begin
  if (FDataLink.Datasource <> nil) and (Datasource.Dataset <> nil) and
    ((not DeleteConfirmation) or (MessageDlg(DeleteMessage, mtConfirmation, mbOKCancel, 0) = mrOk)) then
  begin
    DoBeforeAction(navDelete, DoAction);
    if DoAction then
    begin
      Datasource.Dataset.Delete;
      DoAfterAction(navDelete);
    end;
  end;
end;

procedure TDBNavBarre.SauverBtnClick(Sender: TObject);
var DoAction: Boolean;
  i: Integer;
  LeDS: TDataSource;
begin
  if NavMode = nmSingleDataSource then
  begin
    if (FDataLink.Datasource <> nil) and
      (Datasource.Dataset <> nil) and
      (Datasource.State in [dsEdit, dsInsert]) then
    begin
      DoBeforeAction(navPost, DoAction);
      if DoAction then
      begin
        Datasource.Dataset.Post;
        DoAfterAction(navPost);
      end;
    end;
  end
  else
  begin
    if (DataLinks <> nil) and (DataLinks.Count > 0) then
    begin
      DoBeforeAction(navPost, DoAction);
      if not DoAction then
        Exit;
      for i:= 0 to DataLinks.Count - 1 do
      begin
        try
          LeDS:= TInfoDataLink(DataLinks.Items[i]).DataSource;
          if (LeDS <> nil)
            and (LeDS.DataSet <> nil)
            and (LeDS.State in [dsEdit, dsInsert]) then
          begin
            LeDS.DataSet.Post;
          end;
        except
        end;
      end;
      DoAfterAction(navPost);
    end;
  end;
end;

procedure TDBNavBarre.AnnulerBtnClick(Sender: TObject);
var DoAction: Boolean;
  i: Integer;
  LeDS: TDataSource;
begin
  if NavMode = nmSingleDataSource then
  begin
    if (FDataLink.Datasource <> nil) and
      (Datasource.Dataset <> nil) and
      (Datasource.State in [dsEdit, dsInsert]) then
    begin
      DoBeforeAction(navCancel, DoAction);
      if DoAction then
      begin
        Datasource.Dataset.Cancel;
        DoAfterAction(navCancel);
      end;
    end;
  end
  else
  begin
    if (DataLinks <> nil) and (DataLinks.Count > 0) then
    begin
      DoBeforeAction(navCancel, DoAction);
      if not DoAction then
        Exit;
      for i:= 0 to DataLinks.Count - 1 do
      begin
        try
          LeDS:= TInfoDataLink(DataLinks.Items[i]).DataSource;
          if (LeDS <> nil)
            and (LeDS.DataSet <> nil)
            and (LeDS.State in [dsEdit, dsInsert]) then
          begin
            LeDS.DataSet.Cancel;
          end;
        except
        end;
      end;
      DoAfterAction(navCancel);
    end;
  end;
end;

procedure TDBNavBarre.CreerBoutons;
var
  LeDernier: TBitBtn;
  procedure Ajouter(UnBtn: TBitBtn; Caption, Hint, LeName: string; UnEvent: TNotifyEvent);
  var
    NomGlyph: array[0..40] of Char;
  begin
    Liste2Btn.Add(UnBtn);
    UnBtn.Parent:= Self;
    UnBtn.Caption:= Caption;
    UnBtn.Hint:= Hint;
    if LeName = 'SauverBtn' then
      StrPCopy(NomGlyph, 'BBOK')
    else if LeName = 'AnnulerBtn' then
      StrPCopy(NomGlyph, 'BBCANCEL')
    else
      StrPCopy(NomGlyph, 'FGDBNAVBARRE' + UpperCase(LeName));
    UnBtn.Glyph.Handle:= LoadBitmap(HInstance, NomGlyph);
    UnBtn.NumGlyphs:= 2;
    UnBtn.Enabled:= False;
    UnBtn.Name:= LeName;
    UnBtn.Top:= -100;
    UnBtn.OnClick:= UnEvent;
    if LeDernier <> nil then
      UnBtn.Left:= LeDernier.Left + LeDernier.Width + Spacing
    else
      UnBtn.Left:= 4;
    LeDernier:= UnBtn;
  end;
begin
  Liste2Btn:= TList.Create;
  LeDernier:= nil;
  Ajouter(TBitBtn.Create(Self), InsertLabel, InsertHint, 'AjouterBtn', AjouterBtnClick);
  AjouterBtn:= LeDernier;
  Ajouter(TBitBtn.Create(Self), EditLabel, EditHint, 'ModifierBtn', ModifierBtnClick);
  ModifierBtn:= LeDernier;
  Ajouter(TBitBtn.Create(Self), DeleteLabel, DeleteHint, 'EffacerBtn', EffacerBtnClick);
  EffacerBtn:= LeDernier;
  Ajouter(TBitBtn.Create(Self), PostLabel, PostHint, 'SauverBtn', SauverBtnClick);
  SauverBtn:= LeDernier;
  Ajouter(TBitBtn.Create(Self), CancelLabel, CancelHint, 'AnnulerBtn', AnnulerBtnClick);
  AnnulerBtn:= LeDernier;
  EffacerBtn.Visible:= False;
  AnnulerBtn.Cancel:= True;
  SauverBtn.Default:= True;
  BougerBoutons;
end;

constructor TDBNavBarre.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle - [csSetCaption];
  Caption:= '';
  FLabels:= TStringList.Create;
  FHints:= TStringList.Create;
  FLabels.OnChange:= LabelsChange;
  FHints.OnChange:= HintsChange;
  FVisibleBtn:= [navInsert, navEdit, navPost, navCancel];
  FDeleteConfirmation:= True;
  FNbBtn:= 4;
  FSpacing:= -1;
  FBtnHeight:= -1;
  FBtnWidth:= -1;
  FMarginWidth:= -1;
  FMarginHeight:= -1;
  Width:= 381;
  CreerBoutons;
  FDataLinks:= nil;
  FDSWaiting:= TList.Create;
  FNavMode:= nmSingleDatasource;
  FDataLink:= TInfoDataLink.Create(Self);
end;

procedure TDBNavBarre.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    BougerBoutons;
  if NavMode = nmMultiDataSource then
  begin
    FNavMode:= nmSingleDataSource;
    SetNavMode(nmMultiDatasource);
  end;
end;

destructor TDBNavBarre.Destroy;
var
  i: Integer;
begin
  FLabels.Free;
  FLabels:= nil;
  FHints.Free;
  FHints:= nil;
  FDSWaiting.Free;
  FDSWaiting:= nil;
  if (FDataLinks <> nil) and (FDataLinks.Count > 0) then
  begin
    for i:= 0 to FDataLinks.Count - 1 do
    begin
      TObject(FDataLinks.Items[i]).Free;
    end;
    FDataLinks.Free;
    FDataLinks:= nil;
  end;
  FDataLink.Free;
  FDataLink:= nil;
  Liste2Btn.Free;
  inherited Destroy;
end;

procedure TDBNavBarre.Paint;
begin
 inherited Paint;
end;

procedure TDBNavBarre.SetFocus;
var i: Integer;
begin
  inherited SetFocus;
  if Enabled then
  begin
    for i:= 0 to Liste2Btn.Count - 1 do
    begin
      if TBitBtn(Liste2Btn.Items[i]).Enabled then
      begin
        TBitBtn(Liste2Btn.Items[i]).SetFocus;
        Break;
      end;
    end;
  end;
end;

procedure TDBNavBarre.DataChanged;
begin
  EffacerBtn.Enabled:= Enabled and (FDataLink <> nil) and FDataLink.Active and
    FDataLink.DataSet.CanModify and not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TDBNavBarre.EditingChanged;
var
  CanModify: Boolean;
begin
  if InversionMode then
    BougerBoutons;
  CanModify:= Enabled and Assigned(FDataLink) and Assigned(FDataLink.Dataset) and FDataLink.Active and FDataLink.DataSet.CanModify;
  AjouterBtn.Enabled:= CanModify and not FDataLink.Editing;
  ModifierBtn.Enabled:= CanModify and not FDataLink.Editing;
  EffacerBtn.Enabled:= CanModify and Assigned(FDataLink.Dataset) and not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  SauverBtn.Enabled:= Editing;
  AnnulerBtn.Enabled:= Editing;
end;

procedure TDBNavBarre.ActiveChanged;
begin
  EditingChanged;
end;

function TDBNavBarre.GetDeleteMessage: string;
begin
  if FDeleteMessage = '' then
    Result:= msgDeleteRecord
  else
    Result:= FDeleteMessage;
end;

procedure TDBNavBarre.LabelsChange(Sender: TObject);
var i: Integer;
  S: string;
begin
  for i:= 0 to Liste2Btn.Count - 1 do
  begin
    if (FLabels.Count <= i) or (FLabels[i] = '') then
      S:= StandardLabel(i)
    else
      S:= FLabels[i];
    TBitBtn(Liste2Btn[i]).Caption:= S;
  end;
end;

procedure TDBNavBarre.HintsChange(Sender: TObject);
var i: Integer;
  S: string;
begin
  for i:= 0 to Liste2Btn.Count - 1 do
  begin
    if (FHints.Count <= i) or (FHints[i] = '') then
      S:= StandardHint(i)
    else
      S:= FHints[i];
    TBitBtn(Liste2Btn[i]).Hint:= S;
  end;
end;

function TDBNavBarre.GetLabels: TStrings;
begin
  Result:= FLabels;
end;

function TDBNavBarre.GetHints: TStrings;
begin
  Result:= FHints;
end;

procedure TDBNavBarre.SetLabels(Labels: TStrings);
begin
  if Labels <> nil then
    FLabels.Assign(Labels);
end;

procedure TDBNavBarre.SetHints(Hints: TStrings);
begin
  if Hints <> nil then
    FHints.Assign(Hints);
end;

{ TInfoDataLink }

constructor TInfoDataLink.Create(ADBNavB: TDBNavBarre);
begin
  inherited Create;
  FDBNavBarre:= ADBNavB;
end;

destructor TInfoDataLink.Destroy;
begin
  FDBNavBarre:= nil;
  inherited Destroy;
end;

procedure TInfoDataLink.EditingChanged;
begin
  if FDBNavBarre <> nil then FDBNavBarre.EditingChanged;
end;

procedure TInfoDataLink.DataSetChanged;
begin
  if FDBNavBarre <> nil then FDBNavBarre.DataChanged;
end;

procedure TInfoDataLink.ActiveChanged;
begin
  if FDBNavBarre <> nil then FDBNavBarre.ActiveChanged;
end;

procedure TDBNavBarre.SetCancelEnabled(const Value: Boolean);
begin
  AnnulerBtn.Cancel := Value;
end;

procedure TDBNavBarre.SetDefaultEnabled(const Value: Boolean);
begin
  SauverBtn.Default := Value;
end;

function TDBNavBarre.GetCancelEnabled: Boolean;
begin
  Result := AnnulerBtn.Cancel;
end;

function TDBNavBarre.GetDefaultEnabled: Boolean;
begin
  Result := SauverBtn.Default;
end;

procedure TDBNavBarre.SetNavInsertMonde(const Value: TNavInsertMode);
begin
  FNavInsertMode := Value;
end;

procedure TDBNavBarre.SetEnabled(Value: Boolean);
begin
  inherited;
  EditingChanged;
end;

procedure TDBNavBarre.SetInversionMode(const Value: Boolean);
begin
  if FInversionMode <> Value then
  begin
    FInversionMode := Value;
    BougerBoutons;
  end;
end;

end.

