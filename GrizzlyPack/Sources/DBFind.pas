
unit DBFind;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, Buttons, Menus, FGFindSt;

{ TFindEdit }

type

  TFindMode = (fmNearest, fmMultiWord);

  TSearchEvent = procedure(Sender: TObject; Field: TField; var Text: string;
    var DoSearch: Boolean) of object;
  TFindEvent = procedure(Sender: TObject; Found: Boolean) of object;

  TFindEdit = class(TCustomEdit)
  private
    { Déclarations privées }
    FFieldTagValue: Longint;
    FDataSet: TDataSet;
    FSearchField: string;
    FGoodMatch: Boolean;
    FDefaultDirection: TFindDirection;
    FTimer: TTimer;
    FFindMode: TFindMode;
    FOnSearch: TSearchEvent;
    FOnAfterFind: TFindEvent;
    FOnFieldChange: TNotifyEvent;
    FCalcFields: Boolean;
    procedure SetDataset(Value: TDataset);
    procedure SetSearchField(Value: string);
  protected
    { Déclarations protégées }
    procedure Change; override;
    procedure SetInterval(Value: Cardinal);
    function GetInterval: Cardinal;
    procedure OnTimer(Sender: TObject);
    procedure DoAfterFind(Found: Boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSearchFieldChange; virtual;
  public
    { Déclarations publiques }
    property GoodMatch: Boolean read FGoodMatch;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FindFirst;
    procedure FindLast;
    procedure FindNext;
    procedure FindPrior;
    {}
    function ForceSearch: Boolean;
    function CurrentGoodMatch: Boolean;
  published
    { Déclarations publiées }
    property AllowCalcFields: Boolean read FCalcFields write FCalcFields default True;
    property FindMode: TFindMode read FFindMode write FFindMode default fmNearest;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property SearchField: string read FSearchField write SetSearchField;
    property DefaultDirection: TFindDirection read FDefaultDirection write FDefaultDirection default fdFirst;
    property Interval: Cardinal read GetInterval write SetInterval default 500;
    property FieldTagValue: Longint read FFieldTagValue write FFieldTagValue default 0;
    property BeforeSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnAfterFind: TFindEvent read FOnAfterFind write FOnAfterFind;
    property OnSearchFieldChange: TNotifyEvent read FOnFieldChange write FOnFieldChange;
    property OnChange;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDown;
    property Color default clAqua;
    property Ctl3D;
    property CharCase;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopUpMenu;
    property ShowHint;
    property TabOrder;
    property Text;
    property Visible;
    property Anchors;
  end;

{ TFindPanel }

type
  TFindPanelItems = (viButtons, viComboBox, viLabel);
  TFindPanelVisibleItems = set of TFindPanelItems;

  TVerticalAlignment = (vaTop, vaBottom, vaCenter);

  TFindPanel = class(TCustomPanel)
  private
    { Déclarations privées }
    FTest: Integer;
    FOldAfterOpen: TDataSetNotifyEvent;
    FVisibleItems: TFindPanelVisibleItems;
    FFindEdit: TFindEdit;
    FLabel: TLabel;
    FSplitter: TPanel;
    FComboBoxFields: TComboBox;
    FCurrentComboFields: TStrings;
    FNextBtn, FPriorBtn: TSpeedButton;
    FVerticalAlignment: TVerticalAlignment;
    FChangeIndex: Boolean;
    FOnFieldChange: TNotifyEvent;
    procedure SetInterval(Value: Cardinal);
    function GetInterval: Cardinal;
    procedure SetFieldTagValue(Value: Longint);
    function GetFieldTagValue: Longint;
    procedure SetDataSet(Value: TDataSet);
    procedure SetSearchField(Value: string);
    function GetDataSet: TDataSet;
    function GetSearchField: string;
    procedure SetOnChange(Value: TNotifyEvent);
    function GetOnChange: TNotifyEvent;
    procedure SetDefaultDirection(Value: TFindDirection);
    function GetDefaultDirection: TFindDirection;
    procedure SetFindMode(Value: TFindMode);
    function GetFindMode: TFindMode;
    procedure SetText(Value: string);
    function GetText: string;
    procedure SetCaption(Value: TCaption);
    function GetCaption: TCaption;
    procedure SetCalcFields(Value: Boolean);
    function GetCalcFields: Boolean;
    function GetAfterSearch: TFindEvent;
    procedure SetAfterSearch(Value: TFindEvent);
    procedure SetVerticalAlignment(Value: TVerticalAlignment);
    procedure SetVisibleItems(Value: TFindPanelVisibleItems);
    procedure SetChangeIndex(Value: Boolean);
    procedure AfterOpen(DataSet: TDataSet);
    procedure HookDataSet;
    procedure UnHookDataSet;
    {}
    function GetComboBoxWidth: Integer;
    procedure SetComboBoxWidth(Width: Integer);
    procedure SplitterMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetOnSearch: TSearchEvent;
    procedure SetOnSearch(Ev: TSearchEvent);
    {}
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    { Déclarations protégées }
    procedure ChangerTailles;
    procedure Resize; override;
    procedure Loaded; override;
    procedure DoSearchFieldChange; virtual;
    procedure OnNextClick(Sender: TObject);
    procedure OnPriorClick(Sender: TObject);
    procedure RemplirComboBox(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    {}
    function ForceSearch: Boolean;
    function CurrentGoodMatch: Boolean;
  published
    { Déclarations publiées }
    property AllowCalcFields: Boolean read GetCalcFields write SetCalcFields default True;
    property Caption: TCaption read GetCaption write SetCaption;
    property FindMode: TFindMode read GetFindMode write SetFindMode default fmNearest;
    property DefaultDirection: TFindDirection read GetDefaultDirection write SetDefaultDirection default fdFirst;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default vaCenter;
    property VisibleItems: TFindPanelVisibleItems read FVisibleItems write SetVisibleItems default [viButtons];
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property SearchField: string read GetSearchField write SetSearchField;
    property FieldTagValue: Longint read GetFieldTagValue write SetFieldTagValue default 0;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property Text: string read GetText write SetText;
    property Interval: Cardinal read GetInterval write SetInterval default 500;
    property ChangeIndex: Boolean read FChangeIndex write SetChangeIndex;
    property ComboBoxWidth: Integer read GetComboBoxWidth write SetComboBoxWidth;
    property BeforeSearch: TSearchEvent read GetOnSearch write SetOnSearch;
    property AfterSearch: TFindEvent read GetAfterSearch write SetAfterSearch;
    property OnSearchFieldChange: TNotifyEvent read FOnFieldChange write FOnFieldChange;
    property Align;
    property BevelWidth;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BorderWidth;
    property Ctl3d;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3d;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Anchors;
    property Constraints;
    {}
    property OnEnter;
    property OnExit;
    property Test: Integer read FTest;
  end;

implementation

uses GzConsts, AUtils, UFGDBCtl;

{$R DBFIND32.RES}

{	TFindEdit	}

procedure TFindEdit.SetDataset(Value: TDataset);
begin
  FDataset:= Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TFindEdit.SetSearchField(Value: string);
begin
  if Value <> FSearchField then
  begin
    FSearchField:= Value;
    DoSearchFieldChange;
  end;
end;

procedure TFindEdit.DoSearchFieldChange;
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(Self);
end;

procedure TFindEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
    (Dataset <> nil) and
    (AComponent = Dataset) then
  begin
    Dataset:= nil;
  end;
end;

procedure TFindEdit.DoAfterFind(Found: Boolean);
begin
  if Assigned(FOnAfterFind) then
    FOnAfterFind(Self, Found);
end;

procedure TFindEdit.SetInterval(Value: Cardinal);
begin
  FTimer.Interval:= Value;
end;

function TFindEdit.GetInterval: Cardinal;
begin
  Result:= FTimer.Interval;
end;

procedure TFindEdit.FindFirst;
var S: string;
  DoSearch: Boolean;
  Field: TField;
begin
  FGoodMatch:= False;
  if (SearchField = '') or (Dataset = nil) or (not Dataset.Active) or (Text = '') then
    Exit;
  Field:= DataSet.FieldByName(SearchField);
  if (FindMode = fmMultiWord) or (Field.FieldKind in [fkCalculated, fkLookup]) or
    (Field is TMemoField) then
  begin
    S:= Text;
    DoSearch:= True;
    if Assigned(FOnSearch) then
      FOnSearch(Self, Field, S, DoSearch);
    FGoodMatch:= DoSearch and FindString(S, DataSet, TStringField(Field), fdFirst);
  end
  else
  begin
    FGoodMatch:= False;
    if (DataSet <> nil) then
    begin
      S:= Text;
      DoSearch:= True;
      if Assigned(FOnSearch) then
        FOnSearch(Self, Field, S, DoSearch);
      if not DoSearch then Exit;
      FGoodMatch:= Dataset.Locate(SearchField, S, [loCaseInsensitive, loPartialKey]);
    end;
  end;
  DoAfterFind(FGoodMatch);
end;

procedure TFindEdit.FindLast;
var S: string;
  DoSearch: Boolean;
begin
  FGoodMatch:= False;
  if (SearchField = '') or (Dataset = nil) or (not Dataset.Active) or (Text = '') then
    Exit;
  S:= Text;
  DoSearch:= True;
  if Assigned(FOnSearch) then
    FOnSearch(Self, DataSet.FieldByName(SearchField), S, DoSearch);
  FGoodMatch:= FindString(S, DataSet, TStringField(DataSet.FieldByName(SearchField)), fdLast);
  DoAfterFind(FGoodMatch);
end;

procedure TFindEdit.FindNext;
var S: string;
  DoSearch: Boolean;
begin
  FGoodMatch:= False;
  if (SearchField = '') or (Dataset = nil) or (not Dataset.Active) or (Text = '') then
    Exit;
  S:= Text;
  DoSearch:= True;
  if Assigned(FOnSearch) then
    FOnSearch(Self, DataSet.FieldByName(SearchField), S, DoSearch);
  FGoodMatch:= FindString(S, DataSet, TStringField(DataSet.FieldByName(SearchField)), fdNext);
  DoAfterFind(FGoodMatch);
end;

procedure TFindEdit.FindPrior;
var S: string;
  DoSearch: Boolean;
begin
  FGoodMatch:= False;
  if (SearchField = '') or (Dataset = nil) or (not Dataset.Active) or (Text = '') then
    Exit;
  S:= Text;
  DoSearch:= True;
  if Assigned(FOnSearch) then
    FOnSearch(Self, DataSet.FieldByName(SearchField), S, DoSearch);
  FGoodMatch:= FindString(S, DataSet, TStringField(DataSet.FieldByName(SearchField)), fdPrior);
  DoAfterFind(FGoodMatch);
end;

function TFindEdit.ForceSearch: Boolean;
var S1, S2: string;
  B: Boolean;
begin
  if FTimer.Enabled then OnTimer(FTimer);
  Result:= (DataSet <> nil) and (SearchField <> '') and DataSet.Active and (Text <> '');
  if Result then
    case FindMode of
      fmMultiWord: Result:= MultiWordMatch(Text, DataSet.FieldByName(SearchField).AsString);
      fmNearest:
        begin
          S1:= Text;
          S2:= DataSet.FieldByName(SearchField).AsString;
          B:= True;
          if Assigned(FOnSearch) then
            FOnSearch(Self, DataSet.FieldByName(SearchField), S1, B);
          S2:= Copy(S2, 1, IMin(Length(S1), Length(S2)));
          Result:= CompareText(S1, S2) = 0;
        end;
    end;
  SelectAll;
end;

function TFindEdit.CurrentGoodMatch: Boolean;
begin
  Result:= ForceSearch;
end;

procedure TFindEdit.OnTimer(Sender: TObject);
begin
  FTimer.Enabled:= False;
  case DefaultDirection of
    fdFirst: FindFirst;
    fdNext: FindNext;
    fdPrior: FindPrior;
    fdLast: FindLast;
  end;
end;

procedure TFindEdit.Change;
begin
  FTimer.Enabled:= False;
  FTimer.Enabled:= True;
  inherited Change;
end;

constructor TFindEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop:= True;
  FFindMode:= fmNearest;
  FTimer:= TTimer.Create(Self);
  FTimer.OnTimer:= OnTimer;
  FDataSet:= nil;
  FSearchField:= '';
  FDefaultDirection:= fdFirst;
  Color:= clAqua;
  Interval:= 500;
  FCalcFields:= True;
end;

destructor TFindEdit.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ TFindPanel }

procedure TFindPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
    (Dataset <> nil) and
    (AComponent = Dataset) then
  begin
    Dataset:= nil;
  end;
end;

procedure TFindPanel.SetChangeIndex(Value: Boolean);
begin
  if Value <> FChangeIndex then
  begin
    FChangeIndex:= Value;
  end;
end;

procedure TFindPanel.SetInterval(Value: Cardinal);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.Interval:= Value;
end;

procedure TFindPanel.SetVerticalAlignment(Value: TVerticalAlignment);
begin
  if (Value <> FVerticalAlignment) then
  begin
    FVerticalAlignment:= Value;
    ChangerTailles;
  end;
end;

procedure TFindPanel.SetDefaultDirection(Value: TFindDirection);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.DefaultDirection:= Value;
end;

function TFindPanel.GetDefaultDirection: TFindDirection;
begin
  Result:= fdFirst;
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.DefaultDirection;
end;

procedure TFindPanel.SetFindMode(Value: TFindMode);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.FindMode:= Value;
end;

function TFindPanel.GetFindMode: TFindMode;
begin
  Result:= fmNearest;
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.FindMode;
end;

function TFindPanel.GetInterval: Cardinal;
begin
  Result:= 200;
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.Interval;
end;

procedure TFindPanel.SetFieldTagValue(Value: Longint);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.FieldTagValue:= Value;
end;

function TFindPanel.GetFieldTagValue: Longint;
begin
  Result:= 0;
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.FieldTagValue;
end;

procedure TFindPanel.SetVisibleItems(Value: TFindPanelVisibleItems);
begin
  if (Value <> FVisibleItems) then
  begin
    FVisibleItems:= Value;
    FNextBtn.Visible:= (viButtons in Value);
    FPriorBtn.Visible:= (viButtons in Value);
    FComboBoxFields.Visible:= (viComboBox in Value);
    ChangerTailles;
  end;
end;

procedure TFindPanel.ComboBoxChange(Sender: TObject);
var Idx: Integer;
begin
  if FComboBoxFields = nil then
    Exit;
  Idx:= FComboBoxFields.Items.IndexOf(FComboBoxFields.Text);
  if Idx <> -1 then
    SearchField:= FCurrentComboFields[Integer(FComboBoxFields.Items.Objects[Idx])];
end;

procedure TFindPanel.RemplirComboBox(Sender: TObject);
  procedure GetFields;
  var Field: TField;
    i: Integer;
    function AllowField(Field: TField): Boolean;
    begin
      Result:= Field.Tag = FieldTagValue;
      Result:= Result and ((Field.DataType in [ftString, ftSmallint, ftInteger,
        ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime,
        ftDateTime{$IFDEF WIN32}, ftAutoInc{$ENDIF}, ftWideString, ftLargeint]) or
        ((Field.DataType = ftMemo) and AllowCalcFields));
      Result:= Result and ((Field.FieldKind in [fkData, fkInternalCalc]) or AllowCalcFields);
    end;
  begin
    if Dataset <> nil then
    begin
      for i:= 0 to DataSet.FieldCount - 1 do
      begin
        Field:= DataSet.Fields[i];
        if AllowField(Field) then
        begin
          FCurrentComboFields.Add(Field.FieldName);
          FComboBoxFields.Items.AddObject(Field.DisplayName, Pointer(FCurrentComboFields.Count - 1));
          if Field.FieldName = SearchField then
            FComboBoxFields.Text:= Field.DisplayName;
        end;
      end;
    end;
  end;
begin
  if (FComboBoxFields = nil) then
    Exit;
  try
    FCurrentComboFields.Clear;
    FComboBoxFields.Clear;
    GetFields;
  except
//	  raise;
  end;
end;

procedure TFindPanel.SetText(Value: string);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.Text:= Value;
end;

function TFindPanel.GetText: string;
begin
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.Text;
end;

procedure TFindPanel.OnNextClick(Sender: TObject);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.FindNext;
end;

procedure TFindPanel.OnPriorClick(Sender: TObject);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.FindPrior;
end;

procedure TFindPanel.SetOnChange(Value: TNotifyEvent);
begin
  if FFindEdit = nil then
    Exit;
  FFindEdit.OnChange:= Value;
end;

function TFindPanel.GetOnChange: TNotifyEvent;
begin
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.OnChange;
end;

procedure TFindPanel.UnHookDataSet;
var E1, E2: TDataSetNotifyEvent;
begin
  if Assigned(DataSet) then
  begin
    E1:= AfterOpen;
    E2:= DataSet.AfterOpen;
    if @E1 = @E2 then
      DataSet.AfterOpen:= FOldAfterOpen;
  end;
end;

procedure TFindPanel.HookDataSet;
begin
  if Assigned(DataSet) then
  begin
    FOldAfterOpen:= DataSet.AfterOpen;
    DataSet.AfterOpen:= AfterOpen;
  end;
end;

procedure TFindPanel.AfterOpen;
begin
  if Assigned(DataSet) then
  begin
    if Assigned(FOldAfterOpen) then
      FOldAfterOpen(DataSet);
    RemplirComboBox(FComboBoxFields);
    SetSearchField(SearchField);
  end;
end;

procedure TFindPanel.SetDataSet(Value: TDataSet);
begin
  if FFindEdit = nil then
    Exit;
  UnHookDataSet;
  FFindEdit.DataSet:= Value;
  HookDataSet;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TFindPanel.SetSearchField(Value: string);
var i: Integer;
begin
  if (FFindEdit = nil) or (FComboBoxFields = nil) then
    Exit;
  if FFindEdit.SearchField <> Value then
  begin
    FFindEdit.SearchField:= Value;
    DoSearchFieldChange;
  end;
  FComboBoxFields.Text:= Value;
  for i:= 0 to FComboBoxFields.Items.Count - 1 do
  begin
    if CompareText(FCurrentComboFields[Integer(FComboBoxFields.Items.Objects[i])], Value) = 0 then
    begin
      FComboBoxFields.Text:= FComboBoxFields.Items[i];
      Break;
    end;
  end;
end;

procedure TFindPanel.DoSearchFieldChange;
begin
  if Assigned(FOnFieldChange) then
    FOnFieldChange(Self);
end;

function TFindPanel.GetDataSet: TDataSet;
begin
  Result:= nil;
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.DataSet;
end;

function TFindPanel.GetSearchField: string;
begin
  if FFindEdit = nil then
    Exit;
  Result:= FFindEdit.SearchField;
end;

procedure TFindPanel.SetCaption(Value: TCaption);
begin
  if FLabel = nil then
    Exit;
  if Value <> FLabel.Caption then
  begin
    FLabel.Caption:= Value;
    ChangerTailles;
  end;
end;

function TFindPanel.GetCaption: TCaption;
begin
  if FLabel = nil then
    Exit;
  Result:= FLabel.Caption;
end;

procedure TFindPanel.SetCalcFields(Value: Boolean);
begin
  if Assigned(FFindEdit) then
    FFindEdit.AllowCalcFields:= Value;
end;

function TFindPanel.GetCalcFields: Boolean;
begin
  Result:= False;
  if Assigned(FFindEdit) then
    Result:= FFindEdit.AllowCalcFields;
end;

procedure TFindPanel.ChangerTailles;
var
  TB, G: Longint;
  LEL: Longint;
begin
  if Parent <> nil then
  begin
    FLabel.Visible:= viLabel in VisibleItems;
    FPriorBtn.Visible:= viButtons in VisibleItems;
    FNextBtn.Visible:= viButtons in VisibleItems;
    FComboBoxFields.Visible:= viComboBox in VisibleItems;
    FSplitter.Visible:= viComboBox in VisibleItems;
  end;
  TB:= 0;
  if BevelInner <> bvNone then
    TB:= BevelWidth;
  if BevelOuter <> bvNone then
    TB:= TB + BevelWidth;
  TB:= TB + BorderWidth;
  if BorderStyle <> bsNone then
    TB:= TB + 1;
  LEL:= 0;
  G:= 0;
  if viLabel in VisibleItems then
  begin
    LEL:= LEL + FLabel.Width;
    G:= FLabel.Width;
  end;
  if viButtons in VisibleItems then
    LEL:= LEL + FNextBtn.Width * 2;
  if viComboBox in VisibleItems then
  begin
    LEL:= LEL + FSplitter.Width;
    LEL:= LEL + FComboBoxFields.Width;
  end;
  FLabel.Left:= TB;
  FFindEdit.Width:= Width - TB * 2 - LEL;
  FFindEdit.Left:= TB + G;
  FPriorBtn.Left:= FFindEdit.Left + FFindEdit.Width;
  FNextBtn.Left:= FPriorBtn.Left + FPriorBtn.Width;
  FNextBtn.Height:= FFindEdit.Height;
  FPriorBtn.Height:= FFindEdit.Height;
  if viButtons in VisibleItems then
  begin
    FSplitter.Left:= FNextBtn.Left + FNextBtn.Width;
    FComboBoxFields.Left:= FNextBtn.Left + FNextBtn.Width + FSplitter.Width;
  end
  else
  begin
    FSplitter.Left:= FFindEdit.Left + FFindEdit.Width;
    FComboBoxFields.Left:= FFindEdit.Left + FFindEdit.Width + FSplitter.Width;
  end;
  case VerticalAlignment of
    vaCenter:
      begin
        FLabel.Top:= (Height - FLabel.Height) div 2;
        FFindEdit.Top:= (Height - FFindEdit.Height) div 2;
      end;
    vaTop:
      begin
        FFindEdit.Top:= TB;
        FLabel.Top:= TB + (FFindEdit.Height - FLabel.Height) div 2;
      end;
  else
    begin
      FFindEdit.Top:= Height - TB - FFindEdit.Height;
      FLabel.Top:= FFindEdit.Top + (FFindEdit.Height - FLabel.Height) div 2;
    end;
  end;
  FPriorBtn.Top:= FFindEdit.Top;
  FNextBtn.Top:= FFindEdit.Top;
  FComboBoxFields.Top:= FFindEdit.Top;
end;

const
  MinComboWidth = 60;
  MinEditWidth = 60;

function TFindPanel.GetComboBoxWidth: Integer;
begin
  Result:= FComboBoxFields.Width;
end;

procedure TFindPanel.SetComboBoxWidth(Width: Integer);
begin
  FComboBoxFields.Width:= Width;
  ChangerTailles;
end;

procedure TFindPanel.SplitterMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var NWCombo, NWEdit: Integer;
begin
  if ssLeft in Shift then
  begin
    if (X > FSplitter.Width) then
      X:= X - FSplitter.Width
    else if (X >= 0) then
      X:= 0;
    if X <> 0 then
    begin
      if (FComboBoxFields.Width - X) < MinComboWidth then
        X:= FComboBoxFields.Width - MinComboWidth;
      if (FFindEdit.Width + X) < MinEditWidth then
        X:= MinEditWidth - FFindEdit.Width;
      NWCombo:= FComboBoxFields.Width - X;
      NWEdit:= FFindEdit.Width + X;
      FComboBoxFields.Width:= NWCombo;
      FFindEdit.Width:= NWEdit;
      ChangerTailles;
      Application.ProcessMessages;
    end;
  end;
end;

function TFindPanel.GetOnSearch: TSearchEvent;
begin
  Result:= FFindEdit.BeforeSearch;
end;

procedure TFindPanel.SetOnSearch(Ev: TSearchEvent);
begin
  FFindEdit.BeforeSearch:= Ev;
end;

function TFindPanel.GetAfterSearch: TFindEvent;
begin
  Result:= FFindEdit.OnAfterFind;
end;

procedure TFindPanel.SetAfterSearch(Value: TFindEvent);
begin
  FFindEdit.OnAfterFind:= Value;
end;

procedure TFindPanel.Resize;
begin
  inherited Resize;
  ChangerTailles;
end;

procedure TFindPanel.SetFocus;
begin
  FFindEdit.SetFocus;
end;

procedure TFindPanel.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR, VK_HOME, VK_END]) and (Shift = []) and Assigned(DataSet) and (FFindEdit.Text = '') and (DataSet.Active) then
  begin
    if Key = VK_UP then
      DataSet.Prior
    else if Key = VK_DOWN then
      DataSet.Next
    else if Key = VK_NEXT then
      DataSet.MoveBy(5)
    else if Key = VK_PRIOR then
      DataSet.MoveBy(-5)
    else if Key = VK_HOME then
      DataSet.First
    else if Key = VK_END then
      DataSet.Last;
    Key:= 0;
  end;
end;

procedure TFindPanel.Loaded;
begin
  inherited Loaded;
  Resize;
  RemplirComboBox(FComboboxFields);
  SetSearchField(SearchField);
end;

function TFindPanel.ForceSearch: Boolean;
begin
  Result:= FFindEdit.CurrentGoodMatch;
end;

function TFindPanel.CurrentGoodMatch: Boolean;
begin
  Result:= FFindEdit.CurrentGoodMatch;
end;

constructor TFindPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTest:= 839;
  TabStop:= True;
  FVerticalAlignment:= vaCenter;
  FVisibleItems:= [viButtons];
  FFindEdit:= TFindEdit.Create(Self);
  FFindEdit.Parent:= Self;
  FFindEdit.OnKeyDown:= DoKeyDown;
  FNextBtn:= TSpeedButton.Create(Self);
  FNextBtn.Parent:= Self;
  FNextBtn.Glyph.Handle:= LoadBitmap(HInstance, 'FGFINDEDITNEXT');
  FNextBtn.NumGlyphs:= 2;
  FNextBtn.OnClick:= OnNextClick;
  FNextBtn.Hint:= msgNextOccurence;
  FPriorBtn:= TSpeedButton.Create(Self);
  FPriorBtn.Parent:= Self;
  FPriorBtn.Glyph.Handle:= LoadBitmap(HInstance, 'FGFINDEDITPRIOR');
  FPriorBtn.NumGlyphs:= 2;
  FPriorBtn.OnClick:= OnPriorClick;
  FPriorBtn.Hint:= msgPriorOccurence;
  FComboBoxFields:= TComboBox.Create(Self);
  FComboBoxFields.Parent:= Self;
  FComboBoxFields.OnDropDown:= RemplirComboBox;
  FComboBoxFields.OnChange:= ComboBoxChange;
  FComboBoxFields.Sorted:= True;
  FComboBoxFields.Style:= csDropDown;
  FComboBoxFields.Visible:= False;
  FComboBoxFields.Width:= 85;
  FCurrentComboFields:= TStringList.Create;
  FLabel:= TLabel.Create(Self);
  FLabel.Parent:= Self;
  FLabel.Autosize:= True;
  FLabel.Caption:= ' ' + msgDefaultSearch + ' ';
  FSplitter:= TPanel.Create(Self);
  FSplitter.Parent:= Self;
  FSplitter.Caption:= '';
  FSplitter.Cursor:= crHSplit;
  FSplitter.Width:= 2;
  FSplitter.OnMouseMove:= SplitterMove;
  inherited Caption:= '';
  ControlStyle:= ControlStyle - [csSetCaption];
  Height:= FFindEdit.Height + 4;
end;

destructor TFindPanel.Destroy;
begin
  FCurrentComboFields.Free;
  UnHookDataSet;
  FFindEdit.Free;
  FFindEdit:= nil;
  inherited Destroy;
end;

end.

