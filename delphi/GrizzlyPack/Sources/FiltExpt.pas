unit FiltExpt;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls, DB, Buttons,
  Menus, FGSelec, UFGFilt;

type
  EFilterExpertError = class(Exception);

  TApplySelectionEvent = procedure(Sender: TObject; Selection: string) of object;

  TFillValuesEvent = procedure(Sender: TObject; FieldName: string; List: TStrings) of object;

  TSelectionMode = (smSQL, smFilter);

  TCustomFilterExpert = class(TCustomPanel)
  private
    FTagValue: Longint;
    FFieldList: TStringList;
    FOnApplyEvent: TApplySelectionEvent;
    FDataset: TDataset;
    FSelectionMode: TSelectionMode;
    FSelection: TSelectionList;
    FAutoApply: Boolean;
    FOnFillValues: TFillValuesEvent;
    {}
    procedure SetAutoApply(Value: Boolean);
    procedure SetDataset(Value: TDataset);
    procedure SetSelectionMode(Value: TSelectionMode);
    procedure SetTagValue(Value: Longint);
    procedure SetFieldList(Value: TStrings);
    function GetFieldList: TStrings;
    function GetSelection: string;
    procedure SetSelection(const Value: TSelectionList);
  protected
    procedure AutoFillValues; virtual;
    procedure UserFillValues; virtual;

    function GetValueStrings : TStrings; virtual; abstract;
    function GetCurrentField : TField; virtual; abstract;
    function GetCurrentFieldName : string; virtual; abstract;

    function GetEditValueText: string;  virtual;abstract;
    procedure SetEditValueText(const S: string); virtual; abstract;
    property EditValueText: string read GetEditValueText write SetEditValueText;
    function GetInternalEditValue: string; virtual; abstract;
    procedure SetInternalEditValue(const Value: string); virtual; abstract;
    property InternalEditValue: string read GetInternalEditValue write SetInternalEditValue;

    procedure CreateControls; virtual; abstract;
    procedure MoveControls; virtual; abstract;
    procedure ClearControls; virtual; abstract;

    procedure LoadOperators(ALst : TStrings);
    procedure LoadFields(ALst : TStrings);
    procedure LoadLines(ALst : TStrings);
    procedure LoadLinks(ALst : TStrings);

    procedure UpdateOperators; virtual; abstract;
    procedure UpdateFields; virtual; abstract;
    procedure UpdateLines; virtual; abstract;
    procedure UpdateLinks; virtual; abstract;

    procedure DoBeforeApply; virtual; abstract;
    procedure DoOnApplyEvent(ASelection : string = ''); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;

    procedure Loaded; override;

    property Selection: string read GetSelection;

    procedure ApplySelection;
    procedure ClearSelection;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
  published
    property Dataset: TDataset read FDataset write SetDataset;
    property FieldTagValue: Longint read FTagValue write SetTagValue;
    property Mode: TSelectionMode read FSelectionMode write SetSelectionMode;
    property OnApply: TApplySelectionEvent read FOnApplyEvent write FOnApplyEvent;
    property FieldList: TStrings read GetFieldList write SetFieldList;
    property AutoApply: Boolean read FAutoApply write SetAutoApply;
    property UserSelection: TSelectionList read FSelection write SetSelection;
    property OnFillValues: TFillValuesEvent read FOnFillValues write FOnFillValues;
    {}
    property Align;
    property Visible;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property Ctl3D;
    property ParentCtl3D;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
  end;

  TEditKind = (ekCurrentValues, ekSimpleEdit, ekValueList, ekEditWithButton, ekReadOnlyWithButton);
  TEditButtonClickEvent = procedure(Sender: TObject; FieldName: string; var Value, InternalValue: string) of object;
  TSelectFieldEvent = procedure(Sender: TObject; FieldName: string; var EditKind: TEditKind) of object;

  TFilterExpert = class(TCustomFilterExpert)
  private
    FCBLinks, FCBFields, FCBOperators: TComboBox;
    FEditValue: TWinControl;
    FLBLines: TListBox;
    FBtnAdd,
      FBtnModify,
      FBtnDelete,
      FBtnApply,
      FBtnCancel,
      FBtnAddOpen,
      FBtnRemoveOpen,
      FBtnAddClose,
      FBtnRemoveClose: TBitBtn;
    FEditKind: TEditKind;
    FOnEditWithButtonClick: TEditButtonClickEvent;
    FOnSelectField: TSelectFieldEvent;
    {}
    procedure SetEditKind(Value: TEditKind);
  protected
    function GetEditValueText: string; override;
    procedure SetEditValueText(const S: string); override;
    function GetInternalEditValue: string; override;
    procedure SetInternalEditValue(const Value: string); override;

    procedure DoBeforeApply; override;

    function GetValueStrings : TStrings; override;
    function GetCurrentField : TField; override;
    function GetCurrentFieldName : string; override;
    procedure AutoFillValues; override;

    procedure CreateControls; override;
    procedure MoveControls; override;
    procedure ClearControls; override;

    procedure OnBtnAddClick(Sender: TObject);
    procedure OnBtnModifyClick(Sender: TObject);
    procedure OnBtnDeleteClick(Sender: TObject);
    procedure OnBtnApplyClick(Sender: TObject);
    procedure OnBtnCancelClick(Sender: TObject);
    procedure OnLBLinesClick(Sender: TObject);
    procedure OnEditValueDropDown(Sender: TObject);
    procedure OnEditValueKeyPress(Sender: TObject; var Key: Char);
    procedure OnBtnAddOpenClick(Sender: TObject);
    procedure OnBtnRemoveOpenClick(Sender: TObject);
    procedure OnBtnAddCloseClick(Sender: TObject);
    procedure OnBtnRemoveCloseClick(Sender: TObject);
    {}
    procedure OnCBFieldsChange(Sender: TObject);
    procedure OnEditValueButtonClick(Sender: TObject);

    procedure UpdateOperators; override;
    procedure UpdateLinks; override;
    procedure UpdateFields; override;
    procedure UpdateLines; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure NextLine;
    procedure PriorLine;
    procedure FirstLine;
    procedure LastLine;
  published
    property EditKind: TEditKind read FEditKind write SetEditKind;
    property OnEditWithButtonClick: TEditButtonClickEvent read FOnEditWithButtonClick write FOnEditWithButtonClick;
    property OnSelectField: TSelectFieldEvent read FOnSelectField write FOnSelectField;
  end;

  TPanelWithButton = class(TPanel)
  private
    FEdit: TEdit;
    FButton: TButton;
    FInternalValue: string;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    {}
    property InternalValue: string read FInternalValue write FInternalValue;
    property Edit: TEdit read FEdit;
    property Button: TButton read FButton;
  end;

implementation

{ TCustomFilterExpert }

procedure TCustomFilterExpert.SaveToStream(AStream: TStream);
begin
  FSelection.SaveToStream(AStream);
end;

procedure TCustomFilterExpert.LoadFromStream(AStream: TStream);
begin
  UpdateFields;
  FSelection.LoadFromStream(AStream);
  UpdateLines;
end;

procedure TCustomFilterExpert.ClearSelection;
begin
  FSelection.Clear;
  UpdateLines;
  ClearControls;
end;

procedure TCustomFilterExpert.LoadOperators(ALst : TStrings);
begin
  ALst.Clear;
  if FSelectionMode = smFilter then
    FilterOperators.AssignTo(ALst)
  else
    SQLOperators.AssignTo(ALst)
end;

procedure TCustomFilterExpert.LoadFields(ALst : TStrings);
var i, j: Integer;
    AF : TField;
    AStr1, AStr2: string;
begin
  if csDestroying in ComponentState then
    Exit;
  ALst.Clear;
  if UserSelection.LineCount = 0 then
    FSelection.ClearAll;
  { On remplit à partir de FieldList et on sort, on ignore le Dataset }
  if FieldList.Count > 0 then
  begin
    ALst.Clear;
    for i:= 0 to FieldList.Count - 1 do
    begin
      j:= Pos('=', FieldList[i]);
      if j > 0 then
      begin
        AStr1:= Copy(FieldList[i], 1, j - 1);
        if j < Length(FieldList[i]) then
          AStr2:= Copy(FieldList[i], j + 1, Length(FieldList[i]) - j)
        else
          AStr2:= AStr1;
      end else
      begin
        AStr1:= FieldList[i];
        AStr2:= AStr1;
      end;
      ALst.Add(AStr1);
      AF := DataSet.FindField(AStr2);
      if Assigned(AF) then
      begin
        if AF.Origin <> '' then
          FSelection.AddField(AF.DisplayLabel, AF.Origin, AF)
        else
          FSelection.AddField(AF.DisplayLabel, AF.FieldName, AF);
      end
      else
        FSelection.AddField(AStr1, AStr2, DataSet.FindField(AStr2));
    end;
    Exit;
  end;
  if FDataset = nil then
    Exit;
  if FDataset.FieldCount = 0 then
    Exit;
  for i:= 0 to FDataset.FieldCount - 1 do
  begin
    AF:= FDataSet.Fields[i];
    if (FTagValue = 0) or (FTagValue = AF.Tag) then
    begin
      ALst.Add(AF.DisplayLabel);
      if AF.Origin <> '' then
        FSelection.AddField(AF.DisplayLabel, AF.Origin, AF)
      else
        FSelection.AddField(AF.DisplayLabel, AF.FieldName, AF);
    end;
  end;
end;

procedure TCustomFilterExpert.LoadLinks(ALst : TStrings);
begin
  if ALst.Count > 0 then
    ALst.Clear;
  ALst.Add(Links[0, 1]); { AND }
  ALst.Add(Links[1, 1]); { OR }
end;

procedure TCustomFilterExpert.LoadLines(ALst : TStrings);
var
  i: Integer;
begin
  ALst.BeginUpdate;
  try
    ALst.Clear;
    if FSelection.LineCount > 0 then
      for i:= 0 to FSelection.LineCount - 1 do
      begin
        ALst.Add(FSelection.DisplayLines[i]);
      end;
  finally
    ALst.EndUpDate;
  end;
end;

procedure TCustomFilterExpert.ApplySelection;
begin
  DoBeforeApply;
  if FAutoApply and (FSelectionMode = smFilter) then
  begin
    if FDataset <> nil then
    begin
      FDataset.Filter:= Selection;
      FDataset.Filtered:= True;
    end;
  end;
  DoOnApplyEvent(Selection);
end;

procedure TCustomFilterExpert.SetTagValue(Value: Longint);
begin
  if Value <> FTagValue then
  begin
    FTagValue:= Value;
    UpdateFields;
  end;
end;

procedure TCustomFilterExpert.SetAutoApply(Value: Boolean);
begin
  if Value <> FAutoApply then
  begin
    FAutoApply:= Value;
    if Value then
      ApplySelection;
  end;
end;

procedure TCustomFilterExpert.SetFieldList(Value: TStrings);
begin
  FFieldList.Assign(Value);
  UpdateFields;
end;

function TCustomFilterExpert.GetFieldList: TStrings;
begin
  Result:= FFieldList;
end;

function TCustomFilterExpert.GetSelection: string;
begin
  if FSelectionMode = smFilter then
    Result:= FSelection.SelectionAsFilter
  else
    Result:= FSelection.SelectionAsSQL;
end;

procedure TCustomFilterExpert.SetSelectionMode(Value: TSelectionMode);
begin
  if Value <> FSelectionMode then
  begin
    FSelectionMode:= Value;
    FSelection.Mode:= TOperatorMode(Value);
    UpdateOperators;
    UpdateLines;
  end;
end;

procedure TCustomFilterExpert.SetDataset(Value: TDataset);
begin
  if Value <> FDataset then
  begin
    FDataset:= Value;
    if Value <> nil then
      UpdateFields;
  end;
end;

procedure TCustomFilterExpert.SetSelection(const Value: TSelectionList);
begin
  FSelection.Assign(Value);
end;

procedure TCustomFilterExpert.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)
    and (TDataset(AComponent) = FDataset) then
  begin
    FDataset:= nil;
  end;
end;

procedure TCustomFilterExpert.Resize;
begin
  inherited Resize;
  MoveControls;
end;

procedure TCustomFilterExpert.Loaded;
begin
  inherited Loaded;
  MoveControls;
  UpdateFields;
  UpdateLines;
end;

constructor TCustomFilterExpert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];
  Caption:= '';

  FFieldList:= TStringList.Create;
  FSelection:= TSelectionList.Create;

  FSelectionMode:= smFilter;
  FDataset:= nil;
  FTagValue:= 0;
  FAutoApply:= True;

  Width:= 400;
  Height:= 200;

  CreateControls;
  MoveControls;
end;

procedure TCustomFilterExpert.CreateWnd;
begin
  inherited CreateWnd;
  UpdateFields;
  UpdateOperators;
  UpdateLinks;
end;

destructor TCustomFilterExpert.Destroy;
begin
  Dataset:= nil;
  FSelection.Free;
  FSelection:= nil;
  FFieldList.Free;
  FFieldList:= nil;
  inherited Destroy;
end;

procedure TCustomFilterExpert.AutoFillValues;
var
  ABM: TBookmark;
  Lst: TStringList;
  AF: TField;
  k: Integer;
  ALstToFill : TStrings;
begin
  ALstToFill := GetValueStrings;
  ALstToFill.Clear;
  if (not Assigned(FDataset)) then
    Exit;
  AF:= GetCurrentField;
  if (not Assigned(AF)) then
    Exit;
  Screen.Cursor:= crHourGlass;
  try
    ABM:= FDataset.GetBookmark;
    try
      try
        Lst:= TStringList.Create;
        try
          Lst.Sorted:= True;
          if Assigned(AF) then
          begin
            FDataset.DisableControls;
            try
              FDataset.First;
              while not FDataset.EOF do
              begin
                if not Lst.Find(AF.AsString, k) then
                  Lst.Add(AF.AsString);
                FDataset.Next;
              end;
            finally
              FDataset.EnableControls;
            end;
          end;
          ALstToFill.Assign(Lst);
        finally
          Lst.Free;
        end;
      finally
        FDataset.GotoBookmark(ABM);
      end;
    finally
      FDataset.FreeBookmark(ABM);
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TCustomFilterExpert.UserFillValues;
var
  ALstToFill : TStrings;
begin
  ALstToFill := GetValueStrings;
  ALstToFill.Clear;
  if Assigned(FOnFillValues) then
    FOnFillValues(Self, GetCurrentFieldName, ALstToFill);
end;

procedure TCustomFilterExpert.DoOnApplyEvent(ASelection : string = '');
begin
  if Assigned(FOnApplyEvent) then
    FOnApplyEvent(Self, ASelection);
end;

{ TFilterExpert }

procedure TFilterExpert.NextLine;
begin
  if (FLBLines.Items.Count > 0) and (FLBLines.ItemIndex + 1 < FLBLines.Items.Count) then
  begin
    FLBLines.ItemIndex:= FLBLines.ItemIndex + 1;
    FLBLines.OnClick(FLBLines);
  end;
end;

procedure TFilterExpert.PriorLine;
begin
  if (FLBLines.Items.Count > 0) and (FLBLines.ItemIndex > 0) then
  begin
    FLBLines.ItemIndex:= FLBLines.ItemIndex - 1;
    FLBLines.OnClick(FLBLines);
  end;
end;

procedure TFilterExpert.FirstLine;
begin
  if (FLBLines.Items.Count > 0) then
  begin
    FLBLines.ItemIndex:= 0;
    FLBLines.OnClick(FLBLines);
  end;
end;

procedure TFilterExpert.LastLine;
begin
  if (FLBLines.Items.Count > 0) then
  begin
    FLBLines.ItemIndex:= FLBLines.Items.Count - 1;
    FLBLines.OnClick(FLBLines);
  end;
end;


procedure TFilterExpert.MoveControls;
begin
  FBtnAdd.Left:= 2;
  FBtnModify.Left:= FBtnAdd.Left;
  FBtnDelete.Left:= FBtnAdd.Left;
  FBtnApply.Left:= FBtnAdd.Left;
  FBtnCancel.Left:= FBtnAdd.Left;
  FBtnAddOpen.Left:= FBtnAdd.Left;
  FBtnRemoveOpen.Left:= FBtnAddOpen.Left + FBtnAddOpen.Width;
  FBtnAddClose.Left:= FBtnRemoveOpen.Left + FBtnRemoveOpen.Width + 5;
  FBtnRemoveClose.Left:= FBtnAddClose.Left + FBtnAddClose.Width;
  FBtnAdd.Top:= 2;
  FBtnModify.Top:= FBtnAdd.Top + FBtnAdd.Height + 2;
  FBtnDelete.Top:= FBtnModify.Top + FBtnModify.Height + 2;
  FBtnAddOpen.Top:= FBtnDelete.Top + FBtnDelete.Height + 2;
  FBtnAddClose.Top:= FBtnAddOpen.Top;
  FBtnRemoveOpen.Top:= FBtnAddOpen.Top;
  FBtnRemoveClose.Top:= FBtnAddOpen.Top;
  FBtnApply.Top:= FBtnAddOpen.Top + FBtnAddOpen.Height + 5;
  FBtnCancel.Top:= FBtnApply.Top + FBtnApply.Height + 2;
  FCBLinks.Top:= 2;
  FCBFields.Top:= FCBLinks.Top;
  FCBOperators.Top:= FCBLinks.Top;
  FEditValue.Top:= FCBLinks.Top;
  FLBLines.Top:= FCBLinks.Top + FCBLinks.Height + 2;
  FCBLinks.Left:= FBtnAdd.Left + FBtnAdd.Width + 2;
  FCBFields.Left:= FCBLinks.Left + FCBLinks.Width + 2;
  FCBOperators.Left:= FCBFields.Left + FCBFields.Width + 2;
  FEditValue.Left:= FCBOperators.Left + FCBOperators.Width + 2;
  FEditValue.Width:= Width - 2 - FEditValue.Left;
  FLBLines.Left:= FCBLinks.Left;
  FLBLines.Width:= Width - 2 - FLBLines.Left;
  FLBLines.Height:= Height - 2 - FLBLines.Top;
end;

procedure TFilterExpert.CreateControls;
begin
  FCBLinks:= TComboBox.Create(Self);
  FCBLinks.Parent:= Self;
  FCBLinks.Width:= 50;
  FCBLinks.Style:= csDropDownList;

  FCBFields:= TComboBox.Create(Self);
  FCBFields.Parent:= Self;
  FCBFields.Width:= 180;
  FCBFields.Style:= csDropDownList;
  FCBFields.OnChange:= OnCBFieldsChange;

  FCBOperators:= TComboBox.Create(Self);
  FCBOperators.Parent:= Self;
  FCBOperators.Width:= 130;
  FCBOperators.Style:= csDropDownList;

  if not Assigned(FEditValue) then
  begin
    FEditValue:= TComboBox.Create(Self);
    TComboBox(FEditValue).OnKeyPress:= OnEditValueKeyPress;
    TComboBox(FEditValue).OnDropDown:= OnEditValueDropDown;
  end;
  FEditValue.Parent:= Self;
  FEditValue.Width:= 130;

  FLBLines:= TListBox.Create(Self);
  FLBLines.Parent:= Self;
  FLBLines.Width:= 200;
  FLBLines.OnClick:= OnLBLinesClick;

  FBtnAdd:= TBitBtn.Create(Self);
  FBtnAdd.Parent:= Self;
  FBtnAdd.Width:= 85;
  FBtnAdd.Height:= 25;
  FBtnAdd.OnClick:= OnBtnAddClick;
  FBtnAdd.Caption:= msgAddLine;

  FBtnModify:= TBitBtn.Create(Self);
  FBtnModify.Parent:= Self;
  FBtnModify.Width:= 85;
  FBtnModify.Height:= 25;
  FBtnModify.OnClick:= OnBtnModifyClick;
  FBtnModify.Caption:= msgModifyLine;

  FBtnDelete:= TBitBtn.Create(Self);
  FBtnDelete.Parent:= Self;
  FBtnDelete.Width:= 85;
  FBtnDelete.Height:= 25;
  FBtnDelete.OnClick:= OnBtnDeleteClick;
  FBtnDelete.Caption:= msgDeleteLine;

  FBtnApply:= TBitBtn.Create(Self);
  FBtnApply.Parent:= Self;
  FBtnApply.Width:= 85;
  FBtnApply.Height:= 25;
  FBtnApply.OnClick:= OnBtnApplyClick;
  FBtnApply.Caption:= msgApplySelection;

  FBtnCancel:= TBitBtn.Create(Self);
  FBtnCancel.Parent:= Self;
  FBtnCancel.Width:= 85;
  FBtnCancel.Height:= 25;
  FBtnCancel.OnClick:= OnBtnCancelClick;
  FBtnCancel.Caption:= msgCancelSelection;

  FBtnAddOpen:= TBitBtn.Create(Self);
  FBtnAddOpen.Parent:= Self;
  FBtnAddOpen.Width:= 20;
  FBtnAddOpen.Height:= 25;
  FBtnAddOpen.OnClick:= OnBtnAddOpenClick;
  FBtnAddOpen.Caption:= '(+';
  FBtnAddOpen.Hint:= msgAddOpenBracket;
  FBtnAddOpen.ShowHint:= True;

  FBtnRemoveOpen:= TBitBtn.Create(Self);
  FBtnRemoveOpen.Parent:= Self;
  FBtnRemoveOpen.Width:= 20;
  FBtnRemoveOpen.Height:= 25;
  FBtnRemoveOpen.OnClick:= OnBtnRemoveOpenClick;
  FBtnRemoveOpen.Caption:= '(-';
  FBtnRemoveOpen.Hint:= msgRemoveOpenBracket;
  FBtnRemoveOpen.ShowHint:= True;

  FBtnAddClose:= TBitBtn.Create(Self);
  FBtnAddClose.Parent:= Self;
  FBtnAddClose.Width:= 20;
  FBtnAddClose.Height:= 25;
  FBtnAddClose.OnClick:= OnBtnAddCloseClick;
  FBtnAddClose.Caption:= ')+';
  FBtnAddClose.Hint:= msgAddCloseBracket;
  FBtnAddClose.ShowHint:= True;

  FBtnRemoveClose:= TBitBtn.Create(Self);
  FBtnRemoveClose.Parent:= Self;
  FBtnRemoveClose.Width:= 20;
  FBtnRemoveClose.Height:= 25;
  FBtnRemoveClose.OnClick:= OnBtnRemoveCloseClick;
  FBtnRemoveClose.Caption:= ')-';
  FBtnRemoveClose.Hint:= msgRemoveCloseBracket;
  FBtnRemoveClose.ShowHint:= True;
end;

procedure TFilterExpert.UpdateOperators;
begin
  inherited;
  LoadOperators(FCBOperators.Items);
end;

procedure TFilterExpert.UpdateFields;
begin
  inherited;
  LoadFields(FCBFields.Items);
end;

procedure TFilterExpert.UpdateLinks;
begin
  inherited;
  LoadLinks(FCBLinks.Items);
  FCBLinks.ItemIndex:= 0;
end;

procedure TFilterExpert.UpdateLines;
begin
  inherited;
  LoadLines(FLBLines.Items);
end;

procedure TFilterExpert.OnBtnAddClick(Sender: TObject);
var
  j: Integer;
  k: Integer;
begin
  if (FCBFields.ItemIndex < 0) or (FCBOperators.ItemIndex < 0) then
    Exit;
  k:= FCBLinks.ItemIndex;
  if k < 0 then
    k:= 0;
  j:= FSelection.AddLine(k, FCBFields.ItemIndex, TOperator(FCBOperators.ItemIndex), InternalEditValue, EditValueText);
  UpdateLines;
  FLBLines.ItemIndex:= j;
end;

procedure TFilterExpert.OnBtnModifyClick(Sender: TObject);
var
  TheLine: TSelectionLine;
  k: Integer;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    k:= FCBLinks.ItemIndex;
    if k < 0 then
      k:= 0;
    TheLine.Link:= k;
    TheLine.Field:= FCBFields.ItemIndex;
    TheLine.Operator:= TOperator(FCBOperators.ItemIndex);
    TheLine.DisplayValue:= EditValueText;
    TheLine.Value:= InternalEditValue;
    k:= FLBLines.ItemIndex;
    FLBLines.Items[FLBLines.ItemIndex]:= FSelection.DisplayLines[FLBLines.ItemIndex];
    FLBLines.ItemIndex:= k;
  end;
end;

procedure TFilterExpert.OnBtnDeleteClick(Sender: TObject);
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    FSelection.DeleteLine(FLBLines.ItemIndex);
    if FSelection.LineCount = 0 then
      ClearControls;
  end;
  UpdateLines;
end;

procedure TFilterExpert.OnBtnApplyClick(Sender: TObject);
begin
  ApplySelection;
end;

procedure TFilterExpert.OnBtnCancelClick(Sender: TObject);
begin
  if AutoApply and (FDataset <> nil) and (FSelectionMode = smFilter) then
  begin
{$IFNDEF WIN32}
    raise EFilterExpertError.Create(msgNoFilterIn16Bits);
{$ELSE}
    FDataset.Filter:= '';
{$ENDIF}
  end;
  DoOnApplyEvent;
end;

procedure TFilterExpert.OnLBLinesClick(Sender: TObject);
var
  TheLine: TSelectionLine;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    FCBLinks.ItemIndex:= TheLine.Link;
    FCBFields.ItemIndex:= TheLine.Field;
    FCBOperators.ItemIndex:= Integer(TheLine.Operator);
    EditValueText:= TheLine.DisplayValue;
    InternalEditValue:= TheLine.Value;
  end;
end;

procedure TFilterExpert.OnEditValueDropDown(Sender: TObject);
begin
  if EditKind = ekCurrentValues then
    AutoFillValues
  else if EditKind = ekValueList then
    UserFillValues;
end;

procedure TFilterExpert.OnEditValueKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    OnBtnApplyClick(Sender);
    Key:= #0
  end;
end;

procedure TFilterExpert.OnBtnAddOpenClick(Sender: TObject);
var
  TheLine: TSelectionLine;
  k: Integer;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    TheLine.Brackets.Open;
    k:= FLBLines.ItemIndex;
    UpdateLines;
    FLBLines.ItemIndex:= k;
  end;
end;

procedure TFilterExpert.OnBtnRemoveOpenClick(Sender: TObject);
var
  TheLine: TSelectionLine;
  k: Integer;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    TheLine.Brackets.RemoveOpen;
    k:= FLBLines.ItemIndex;
    UpdateLines;
    FLBLines.ItemIndex:= k;
  end;
end;

procedure TFilterExpert.OnBtnAddCloseClick(Sender: TObject);
var
  TheLine: TSelectionLine;
  k: Integer;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    TheLine.Brackets.Close;
    k:= FLBLines.ItemIndex;
    UpdateLines;
    FLBLines.ItemIndex:= k;
  end;
end;

procedure TFilterExpert.OnBtnRemoveCloseClick(Sender: TObject);
var
  TheLine: TSelectionLine;
  k: Integer;
begin
  if (FLBLines.ItemIndex >= 0) and (FLBLines.ItemIndex < FSelection.LineCount) then
  begin
    TheLine:= FSelection.Lines[FLBLines.ItemIndex];
    TheLine.Brackets.RemoveClose;
    k:= FLBLines.ItemIndex;
    UpdateLines;
    FLBLines.ItemIndex:= k;
  end;
end;

function TFilterExpert.GetCurrentField: TField;
var
  i: Integer;
  AFN: string;
begin
  if (FieldList.Count > 0) and (Assigned(FDataset)) then
  begin
    AFN:= GetCurrentFieldName;
    for i:= 0 to FDataset.FieldCount - 1 do
    begin
      Result:= FDataset.Fields[i];
      if CompareText(Trim(Result.Origin), Trim(AFN)) = 0 then
        Exit;
    end;
    Result:= FDataset.FindField(AFN);
  end else
    Result:= FSelection.Fields[FCBFields.ItemIndex];
end;

constructor TFilterExpert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditKind:= ekCurrentValues;
end;

procedure TFilterExpert.CreateWnd;
begin
  inherited CreateWnd;
  if FEditValue is TComboBox then
    TComboBox(FEditValue).Sorted:= True;
end;

procedure TFilterExpert.OnEditValueButtonClick(Sender: TObject);
var S, Internal: string;
begin
  S:= TPanelWithButton(FEditValue).Edit.Text;
  Internal:= TPanelWithButton(FEditValue).InternalValue;
  if Assigned(FOnEditWithButtonClick) then
    FOnEditWithButtonClick(Self, FSelection.FieldNames[FCBFields.ItemIndex], S, Internal);
  TPanelWithButton(FEditValue).Edit.Text:= S;
  TPanelWithButton(FEditValue).InternalValue:= Internal;
end;

procedure TFilterExpert.OnCBFieldsChange(Sender: TObject);
var E: TEditKind;
begin
  if Assigned(FOnSelectField) then
  begin
    E:= EditKind;
    FOnSelectField(Self, FSelection.FieldNames[FCBFields.ItemIndex], E);
    if E <> EditKind then
      EditKind:= E;
  end;
end;

procedure TFilterExpert.SetEditKind(Value: TEditKind);
  procedure CreateComboBox;
  begin
    FEditValue:= TComboBox.Create(Self);
    TComboBox(FEditValue).Style:= csDropDown;
    TComboBox(FEditValue).OnDropDown:= OnEditValueDropDown;
    TComboBox(FEditValue).OnKeyPress:= OnEditValueKeyPress;
  end;
  procedure CreateEdit;
  begin
    FEditValue:= TEdit.Create(Self);
    TEdit(FEditValue).OnKeyPress:= OnEditValueKeyPress;
  end;
  procedure CreateEditWithButton;
  begin
    FEditValue:= TPanelWithButton.Create(Self);
    {}
    TPanelWithButton(FEditValue).Edit.OnKeyPress:= OnEditValueKeyPress;
    TPanelWithButton(FEditValue).Button.OnClick:= OnEditValueButtonClick;
  end;
begin
  FEditKind:= Value;
  {}
  FEditValue.Free;
  FEditValue:= nil;
  case Value of
    ekCurrentValues: CreateComboBox;
    ekSimpleEdit: CreateEdit;
    ekValueList: CreateComboBox;
    ekEditWithButton: CreateEditWithButton;
    ekReadOnlyWithButton:
    begin
      CreateEditWithButton;
      with TPanelWithButton(FEditValue).Edit do
        ReadOnly:= True;
    end;
  end;
  FEditValue.Parent:= Self;
  FEditValue.Width:= 130;
  {}
  if not (csLoading in ComponentState) then
  begin
    FEditValue.Top:= FCBLinks.Top;
    FEditValue.Left:= FCBOperators.Left + FCBOperators.Width + 2;
    FEditValue.Width:= Width - 2 - FEditValue.Left;
    {}
    if EditKind = ekReadOnlyWithButton then
      TPanelWithButton(FEditValue).Edit.Font.Color:= clBlue;
  end;
end;

function TFilterExpert.GetEditValueText: string;
begin
  if FEditValue is TComboBox then
    Result:= TComboBox(FEditValue).Text
  else if FEditValue is TEdit then
    Result:= (FEditValue as TEdit).Text
  else if FEditValue is TPanelWithButton then
    Result:= (FEditValue as TPanelWithButton).Edit.Text;
end;

procedure TFilterExpert.SetEditValueText(const S: string);
begin
  if FEditValue is TComboBox then
    TComboBox(FEditValue).Text:= S
  else if FEditValue is TEdit then
    (FEditValue as TEdit).Text:= S
  else if FEditValue is TPanelWithButton then
    (FEditValue as TPanelWithButton).Edit.Text:= S;
end;

function TFilterExpert.GetInternalEditValue: string;
begin
  if FEditValue is TPanelWithButton then
    Result:= (FEditValue as TPanelWithButton).InternalValue
  else
    Result:= '';
  if Result = '' then
    Result:= EditValueText;
end;

procedure TFilterExpert.SetInternalEditValue(const Value: string);
begin
  if FEditValue is TPanelWithButton then
    (FEditValue as TPanelWithButton).InternalValue:= Value
  else
    EditValueText:= Value;
end;

function TFilterExpert.GetCurrentFieldName: string;
begin
  Result:= FSelection.FieldNames[FCBFields.ItemIndex];
end;

function TFilterExpert.GetValueStrings: TStrings;
begin
  Result := (FEditValue as TComboBox).Items;
end;

procedure TFilterExpert.AutoFillValues;
begin
  if (FCBFields.ItemIndex < 0) then
    Exit;
  (FEditValue as TComboBox).SetFocus;
  inherited;
end;

procedure TFilterExpert.DoBeforeApply;
begin
  inherited;
  if FSelection.LineCount = 0 then
    OnBtnAddClick(Self);
end;

procedure TFilterExpert.ClearControls;
begin
  inherited;
  FCBLinks.ItemIndex := 0;
  FCBFields.ItemIndex := -1;
  FCBOperators.ItemIndex := -1;
  TComboBox(FEditValue).Text := '';
end;

{ TPanelWithButton }

constructor TPanelWithButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {}
  FEdit:= TEdit.Create(Self);
  FButton:= TButton.Create(Self);
  {}
  BevelOuter:= bvNone;
  {}
  FButton.Caption:= 'ooo';
  FButton.Font.Name:= 'Arial';
  FButton.Font.Size:= 6;
  FButton.Font.Style:= [fsBold];
  FButton.Width:= 25;
  {}
  FEdit.Parent:= Self;
  FButton.Parent:= Self;
  {}
  Height:= FEdit.Height + 3; // Pkoi le + 3 ? A comprendre un jour si on a le temps
end;

procedure TPanelWithButton.Resize;
begin // Repositionnement de l'edit et du bouton
  Button.Height:= Edit.Height - 1;
  Edit.Width:= ClientWidth - Button.Width - 3;
  Edit.Left:= 0;
  Edit.Top:= 0;
  Button.Left:= Edit.Width + 1;
  Button.Top:= 1;
end;

end.

