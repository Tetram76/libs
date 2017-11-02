unit GzHTMLEditor;

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Forms, OleCtrls, ProfDHTMLEdit,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterHtml;

type
  THTMLEditor = class;

  TSynMemoWrapper = class(TPersistent)
  private
    FSynMemo : TSynMemo;
    FHTMLEditor : THTMLEditor;
    function GetHeight: Integer;
    function GetReadOnly: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Height : Integer read GetHeight write SetHeight;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly default False;
    property Visible : Boolean read GetVisible write SetVisible default True;
  end;

  TDHTMLEditWrapper = class(TPersistent)
  private
    FDHTMLEdit : TProfDHTMLEdit;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetSourceCodePreservation: Boolean;
    procedure SetSourceCodePreservation(const Value: Boolean);
    function GetSourceCodeOptions: TProfDHTMLEditSourceCodeOptions;
    procedure SetSourceCodeOptions(const Value: TProfDHTMLEditSourceCodeOptions);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly default False;
    property SourceCodePreservation : Boolean read GetSourceCodePreservation write SetSourceCodePreservation default False;
    property SourceCodeOptions : TProfDHTMLEditSourceCodeOptions read GetSourceCodeOptions write SetSourceCodeOptions
    default [
      LowercaseElementNames,
      TerminateComments,
      LowercaseAttributeNames,
      QuoteAttributeValues,
      UnminimizeBooleanAttributes,
      UseEntityReferences,
      Preserve8BitCharacters,
      ExcludeFileProtocol,
      ExcludeBaseURL
    ];

  end;

  TFillHeaderEvent =  procedure (Sender: TObject; var Title, CSS : string) of object;

  THeaderManager = class(TPersistent)
  private
    FHTMLEditor : THTMLEditor;
    FActive: Boolean;
    FHeaders: TStringList;
    FTitle: string;
    FCSS: string;
    procedure SetHeaders(const Value: TStringList);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active : Boolean read FActive write FActive default True;
    property Headers : TStringList read FHeaders write SetHeaders;
    property DefaultTitle : string read FTitle write FTitle;
    property DefaultCSS : string read FCSS write FCSS;
  end;

  THTMLEditor = class(TCustomPanel)
  private
    { Déclarations privées }
    FMemoWysiwyg: TProfDHTMLEdit;
    FSplitter: TSplitter;
    FMemoHTML: TSynMemo;
    FHTMLHighlight: TSynHTMLSyn;
    FOnChange: TNotifyEvent;
    FHTMLWrapper: TSynMemoWrapper;
    FWysiwygWrapper: TDHTMLEditWrapper;
    FMemoChanging,
    FWysiwygChanging,
    FInitializing : Boolean;
    FHeaderManager: THeaderManager;
    FOnFillHeader: TFillHeaderEvent;
    FOnDisplayChanged: TNotifyEvent;
    procedure SetHTMLWrapper(const Value: TSynMemoWrapper);
    procedure SetWysiwygWrapper(const Value: TDHTMLEditWrapper);
    procedure SetHeaderManager(const Value: THeaderManager);
    function GetHTML: string;
    procedure SetHTML(const Value: string);

    procedure MemoHTMLChange(Sender: TObject);
    procedure MemoHTMLExit(Sender: TObject);

    procedure MemoWysiwygDisplayChanged(Sender: TObject);
    procedure MemoWysiwygExit(Sender: TObject);
    procedure MemoWysiwygCreate(Sender: TObject);
    procedure MemoWysiwygSetSource(Sender: TObject);
  protected
    { Déclarations protégées }
    procedure CreateControls;
    procedure HookEvents;

    procedure DoOnChange;

    function HTMLToWysiwyg : string;
    function WysiwygToHTML : string;
    procedure SynchronizeMemoHTML;
    procedure SynchronizeMemoWysiwyg;
  public
    { Déclarations publiques }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MemoHTML : TSynMemo read FMemoHTML;
    property MemoWysiwyg : TProfDHTMLEdit read FMemoWysiwyg;

    property HTML : string read GetHTML write SetHTML;
  published
    { Déclarations publiées }
    property Align;
    property BorderWidth default 1;
    property BevelWidth;
    property BevelInner;
    property BevelOuter default bvLowered;
    property TabStop;
    property Visible;
    property Enabled;
    property WysiwygWrapper : TDHTMLEditWrapper read FWysiwygWrapper write SetWysiwygWrapper;
    property HTMLWrapper : TSynMemoWrapper read FHTMLWrapper write SetHTMLWrapper;
    property HeaderManager : THeaderManager read FHeaderManager write SetHeaderManager;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnFillHeader : TFillHeaderEvent read FOnFillHeader write FOnFillHeader;
    property OnDisplayChanged : TNotifyEvent read FOnDisplayChanged write FOnDisplayChanged;
  end;

procedure Register;

implementation

uses GzConsts, FGUtils;

procedure Register;
begin
  RegisterComponents(SGrizzlyTools, [THTMLEditor]);
end;

const
  BodyStart = '<body>';
  BodyEnd = '</body>';

function ExtractBody(const AHTML : string) : string;
begin
  Result := AHTML;
  Result := StringReplace(Result, BodyStart, BodyStart, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, BodyEnd, BodyEnd, [rfReplaceAll, rfIgnoreCase]);
  if Pos(BodyEnd, Result) > 0 then
  begin
    if Pos(BodyStart, Result) > 0 then
      Result := GetDelimitedString(BodyStart, BodyEnd, Result)
    else
    begin
      Result := StringReplace(Result, '<body', '<body', [rfReplaceAll, rfIgnoreCase]);
      if Pos('<body', Result) > 0 then
      begin
        Result := GetDelimitedString('<body', BodyEnd, Result) + BodyEnd;
        Result := GetDelimitedString('>', BodyEnd, Result);
      end;
    end;
  end;
end;

{ TSynMemoWrapper }

procedure TSynMemoWrapper.Assign(Source: TPersistent);
begin
  if Source is TSynMemoWrapper then
    with TSynMemoWrapper(Source) do
    begin
      Self.Height := Height;
      Self.ReadOnly := ReadOnly;
    end
  else
    inherited;
end;

constructor TSynMemoWrapper.Create(AOwner: TComponent);
begin
  FHTMLEditor := THTMLEditor(AOwner);
  FSynMemo := FHTMLEditor.FMemoHTML;
end;

function TSynMemoWrapper.GetHeight: Integer;
begin
  Result := FSynMemo.Height;
end;

function TSynMemoWrapper.GetReadOnly: Boolean;
begin
  Result := FSynMemo.ReadOnly;
end;

function TSynMemoWrapper.GetVisible: Boolean;
begin
  Result := FSynMemo.Visible;
end;

procedure TSynMemoWrapper.SetHeight(const Value: Integer);
begin
  FSynMemo.Height := Value;
end;

procedure TSynMemoWrapper.SetReadOnly(const Value: Boolean);
begin
  FSynMemo.ReadOnly := Value;
end;

procedure TSynMemoWrapper.SetVisible(const Value: Boolean);
begin
  FSynMemo.Visible := Value;
  FHTMLEditor.FSplitter.Visible := Value;
  FHTMLEditor.FSplitter.Top := FHTMLEditor.Height - FSynMemo.Height;
end;

{ TDHTMLEditWrapper }

procedure TDHTMLEditWrapper.Assign(Source: TPersistent);
begin
  if Source is TProfDHTMLEdit then
    with TProfDHTMLEdit(Source) do
    begin
      Self.ReadOnly := ReadOnly;
    end
  else
    inherited;
end;

constructor TDHTMLEditWrapper.Create(AOwner: TComponent);
begin
  FDHTMLEdit := THTMLEditor(AOwner).FMemoWysiwyg;
end;

function TDHTMLEditWrapper.GetReadOnly: Boolean;
begin
  Result := not FDHTMLEdit.Enabled;
end;

function TDHTMLEditWrapper.GetSourceCodeOptions: TProfDHTMLEditSourceCodeOptions;
begin
  Result := FDHTMLEdit.SourceCodeOptions;
end;

function TDHTMLEditWrapper.GetSourceCodePreservation: Boolean;
begin
  Result := FDHTMLEdit.SourceCodePreservation;
end;

procedure TDHTMLEditWrapper.SetReadOnly(const Value: Boolean);
begin
  FDHTMLEdit.Enabled := not Value;
end;

procedure TDHTMLEditWrapper.SetSourceCodeOptions(const Value: TProfDHTMLEditSourceCodeOptions);
begin
  FDHTMLEdit.SourceCodeOptions := Value;
end;

procedure TDHTMLEditWrapper.SetSourceCodePreservation(const Value: Boolean);
begin
  FDHTMLEdit.SourceCodePreservation := Value;
end;

{ THeaderManager }

procedure THeaderManager.Assign(Source: TPersistent);
begin
  if Source is THeaderManager then
    with THeaderManager(Source) do
    begin
      Self.Active := Active;
      Self.Headers.Text := Trim(Headers.Text);
    end
  else
    inherited;
end;

constructor THeaderManager.Create(AOwner: TComponent);
begin
  FHTMLEditor := THTMLEditor(AOwner);
  FActive := True;
  FHeaders := TStringList.Create;
  FHeaders.Text :=
    '<title>%TITRE%</title>' + #13#10 +
    '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' + #13#10 +
    '<link rel="stylesheet" href="%CSS%" type="text/css">' + #13#10;
end;

destructor THeaderManager.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure THeaderManager.SetHeaders(const Value: TStringList);
begin
  FHeaders.Text := Trim(Value.Text);
end;

{ THTMLEditor }

constructor THTMLEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInitializing := True;
  FMemoChanging := False;
  FWysiwygChanging := False;

  ControlStyle:= ControlStyle - [csSetCaption];
  Caption:= '';
  BorderWidth := 1;
  BevelOuter := bvLowered;
  Height := 300;
  Width := 300;
  FHeaderManager := THeaderManager.Create(Self);
  CreateControls;
  FHTMLWrapper := TSynMemoWrapper.Create(Self);
  FWysiwygWrapper := TDHTMLEditWrapper.Create(Self);

  HookEvents;
end;

procedure THTMLEditor.CreateControls;
  procedure InitControl(AControl : TControl; AName : string);
  begin
    AControl.Name := AName;
    AControl.Parent := Self;
  end;
begin

  FHTMLHighlight := TSynHTMLSyn.Create(Self);

  FMemoHTML := TSynMemo.Create(Self);
  InitControl(FMemoHTML, 'MemoHTML');
  FMemoHTML.Height := 100;
  FMemoHTML.Align := alBottom;
  FMemoHTML.Highlighter := FHTMLHighLight;
  FMemoHTML.Text := '';
  FMemoHTML.WordWrap := True;
  FMemoHTML.WantTabs := True;
  FMemoHTML.WantReturns := True;


  FSplitter := TSplitter.Create(Self);
  InitControl(FSplitter, 'Splitter');
  FSplitter.Align := alBottom;
  FSplitter.Top := FMemoHTML.Top - 5;

  FMemoWysiwyg := TProfDHTMLEdit.Create(Self);
  InitControl(FMemoWysiwyg, 'MemoWysiwyg');
  FMemoWysiwyg.Height := 100;
  FMemoWysiwyg.Align := alClient;
  FMemoWysiwyg.SourceCodePreservation := False;
  FMemoWysiwyg.SourceCodeOptions :=
    [LowercaseElementNames,TerminateComments,LowercaseAttributeNames,
     QuoteAttributeValues,UnminimizeBooleanAttributes,UseEntityReferences,
     Preserve8BitCharacters,ExcludeFileProtocol,ExcludeBaseURL];
  FMemoWysiwyg.ShowBorders := True;
  FMemoWysiwyg.ShowDetails := True;
  FMemoWysiwyg.ShowBrTags := True;
  FMemoWysiwyg.Timeout := 1;
  
end;

procedure THTMLEditor.HookEvents;
begin
  FMemoWysiwyg.OnDisplayChanged := MemoWysiwygDisplayChanged;
  FMemoWysiwyg.OnCreate := MemoWysiwygCreate;
  FMemoWysiwyg.OnExit := MemoWysiwygExit;
  FMemoWysiwyg.OnSetSource := MemoWysiwygSetSource;
  FMemoWysiwyg.OnLoadFromFile := MemoWysiwygSetSource;
  FMemoWysiwyg.OnLoadURL := MemoWysiwygSetSource;
  FMemoWysiwyg.OnOpenDialog := MemoWysiwygSetSource;
  FMemoWysiwyg.OnNewDocument := MemoWysiwygSetSource;

  FMemoHTML.OnChange := MemoHTMLChange;
  FMemoHTML.OnExit := MemoHTMLExit;
end;

destructor THTMLEditor.Destroy;
begin
  FHeaderManager.Free;
  FHTMLWrapper.Free;
  FWysiwygWrapper.Free;

  inherited;
end;

procedure THTMLEditor.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THTMLEditor.Loaded;
begin
  inherited;
  FMemoHTML.Text := '';
  FInitializing := False;
end;

procedure THTMLEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure THTMLEditor.SetHTMLWrapper(const Value: TSynMemoWrapper);
begin
  FHTMLWrapper.Assign(Value);
end;

procedure THTMLEditor.SetWysiwygWrapper(const Value: TDHTMLEditWrapper);
begin
  FWysiwygWrapper.Assign(Value);
end;

procedure THTMLEditor.SetHeaderManager(const Value: THeaderManager);
begin
  FHeaderManager.Assign(Value);
end;

procedure THTMLEditor.MemoHTMLChange(Sender: TObject);
begin
  if FInitializing or FWysiwygChanging then
    Exit;
  MemoWysiwyg.BeginUpdate;
  try
    SynchronizeMemoWysiwyg;
  finally
    MemoWysiwyg.EndUpdate;
  end;
  DoOnChange;
end;

procedure THTMLEditor.MemoWysiwygDisplayChanged(Sender: TObject);
begin
  if Assigned(FOnDisplayChanged) then
    FOnDisplayChanged(MemoWysiwyg);
  if FInitializing or FMemoChanging then
    Exit;
  if MemoWysiwyg.Modified then
  begin
    SynchronizeMemoHTML;
    DoOnChange;
  end;
end;

procedure THTMLEditor.SynchronizeMemoHTML;
begin
  FWysiwygChanging := True;
  try
    MemoWysiwyg.Modified := False;
    MemoHTML.Text := WysiwygToHTML;
  finally
    FWysiwygChanging := False;
  end;
end;

procedure THTMLEditor.SynchronizeMemoWysiwyg;
var
  Debut : TDateTime;
  Delai : TDateTime;
begin
  FMemoChanging := True;
  try
    if MemoWysiwyg.Busy then
    begin
      //MemoWysiwyg.Stop;
      Delai := SecondToDateTime(1) / 4;
      Debut := Now;
      while MemoWysiwyg.Busy do
      begin
        Application.ProcessMessages;
        Sleep(10);
        if Now - Debut > Delai then
        begin
          Break;
        end;
      end;
    end;
    MemoWysiwyg.Source := HTMLToWysiwyg;
  finally
    FMemoChanging := False;
  end;
end;

function THTMLEditor.HTMLToWysiwyg: string;
var
  AHTML, ATitle, ACSS : string;
const
  Entete =  '<html>' + #13#10 +
            '<head>' + #13#10 +
            '%ENTETE%' + #13#10 +
            '</head>' + #13#10 +
            BodyStart;
  Pied =    BodyEnd + #13#10 +
            '</html>';
begin
  if not HeaderManager.Active then
    Result := MemoHTML.Text
  else
  begin
    AHTML := ExtractBody(MemoHTML.Text);
    AHTML := StringReplace(Entete, '%ENTETE%', Trim(HeaderManager.Headers.Text), [rfReplaceAll, rfIgnoreCase])
             + AHTML + Pied;

    ATitle := HeaderManager.DefaultTitle;
    ACSS := HeaderManager.DefaultCSS;
    if Assigned(FOnFillHeader) then
      FOnFillHeader(Self, ATitle, ACSS);
    AHTML := StringReplace(AHTML, '%TITRE%', ATitle, [rfReplaceAll, rfIgnoreCase]);
    AHTML := StringReplace(AHTML, '%CSS%', ACSS, [rfReplaceAll, rfIgnoreCase]);
    Result := AHTML;
  end;
end;

function THTMLEditor.WysiwygToHTML: string;
begin
  if not HeaderManager.Active then
    Result := MemoWysiwyg.Source
  else
    Result := ExtractBody(MemoWysiwyg.Source);
end;

procedure THTMLEditor.MemoHTMLExit(Sender: TObject);
begin
{  if MemoHTML.Modified then
    SynchronizeMemoWysiwyg; }  //Crée une erreur dans l'éditeur dhtml...
end;

procedure THTMLEditor.MemoWysiwygExit(Sender: TObject);
begin
{  if MemoWysiwyg.Modified then
    SynchronizeMemoHTML; }     //Crée une erreur dans l'éditeur dhtml...
end;

function THTMLEditor.GetHTML: string;
begin
  Result := MemoHTML.Text;
end;

procedure THTMLEditor.SetHTML(const Value: string);
begin
  MemoHTML.Text := Value;
  SynchronizeMemoWysiwyg;
end;

procedure THTMLEditor.MemoWysiwygCreate(Sender: TObject);
begin
  SynchronizeMemoWysiwyg;
end;

procedure THTMLEditor.MemoWysiwygSetSource(Sender: TObject);
begin
  MemoWysiwyg.Modified := True;
  MemoWysiwygDisplayChanged(Sender);
end;

end.
