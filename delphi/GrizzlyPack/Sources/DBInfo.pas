unit DBInfo;

interface

{$IFDEF WIN32}
{$R DBINFO32.RES}
{$ELSE}
{$R DBINFO.RES}
{$ENDIF}

uses
  WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, SysUtils, DB {$IFNDEF WIN32}, DBTables{$ENDIF}{$IFDEF VER80}, Menus{$ENDIF};

type
	TCustomDBInformation = Class;

	TInfoDataLink = class(TDataLink)
  private
    FDBInformation: TCustomDBInformation;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ADBInf : TCustomDBInformation);
    destructor Destroy; override;
  end;

  TCustomDBInformation = class(TCustomPanel)
  private
    { Déclarations private }
    FDataLink : TInfoDataLink;
  protected
    { Déclarations protected }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetDataSource(Value : TDataSource);
    Function GetDataSource : TDataSource;
    procedure EditingChanged; virtual;
    procedure DatasetChanged; virtual;
    procedure ActiveChanged; virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure CheckBrowseMode; virtual;
    Procedure ChangeCaption; virtual;
    Procedure ClearCaption; virtual;
  public
    { Déclarations public }
    Constructor Create(AOwner : TComponent); override;
    Procedure Loaded; override;
		Destructor Destroy; override;
  published
    { Déclarations published }
    Property DataSource : TDataSource Read GetDataSource Write SetDataSource;
    Property Align;
    Property Alignment;
    Property BevelInner;
    Property BevelOuter;
    Property BevelWidth;
    Property BorderStyle;
    Property BorderWidth;
    Property Color;
    Property Ctl3D;
    Property Font;
    Property ParentColor;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property Visible;
    Property Enabled;
    Property TabStop;
    Property TabOrder;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnResize;
		{$IFNDEF VER80}
    Property OnStartDrag;
    {$ENDIF}
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
  end;

  TDBRecordCount = class(TCustomDBInformation)
  private
    { Déclarations private }
    FRecordName : String;
    FPluralLetter : String;
    procedure SetRecordName(const Value: string);
    procedure SetPluralLetter(const Value: string);
  protected
    { Déclarations protected }
    procedure DatasetChanged; override;
  public
    { Déclarations public }
    Constructor Create(AOwner : TComponent); override;
    Procedure ChangeCaption; override;
  published
    { Déclarations published }
    Property RecordName : String Read FRecordName Write SetRecordName;
    Property PluralLetter : String Read FPluralLetter Write SetPluralLetter;
  end;

  TDBState = class(TCustomDBInformation)
  private
    { Déclarations private }
    FState : TDataSetState;
  protected
    { Déclarations protected }
  public
    { Déclarations public }
    Procedure ChangeCaption; override;
  published
    { Déclarations published }
  end;

procedure Register;


implementation

Uses GzConsts, FGUtils, UFGDBCtl;

{ TCustomDBInformation }

Constructor TCustomDBInformation.Create(AOwner : TComponent);
Begin
	Inherited Create(AOwner);
  Caption:='';
  ControlStyle := ControlStyle - [csSetCaption];
	FDataLink:=TInfoDataLink.Create(Self);
End;

Procedure TCustomDBInformation.Loaded;
Begin
	Inherited Loaded;
End;

Destructor TCustomDBInformation.Destroy;
Begin
	FDataLink.Free;
  FDataLink:=Nil;
  Inherited Destroy;
End;

Procedure TCustomDBInformation.SetDataSource(Value : TDataSource);
Begin
	FDataLink.DataSource:=Value;
{$IFDEF WIN32}
  If Value<>Nil then
    Value.FreeNotification(Self);
{$ENDIF}
End;

Function TCustomDBInformation.GetDataSource : TDataSource;
Begin
	Result:=FDataLink.DataSource;
End;

Procedure TCustomDBInformation.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  Inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and
    (FDatalink<>Nil) and
    (AComponent=FDataLink.DataSource) then
  Begin
    FDatalink.DataSource:=Nil;
  End;
End;

Procedure TCustomDBInformation.ClearCaption;
Begin
	Caption:='';
End;

Procedure TCustomDBInformation.ChangeCaption;
Begin
End;

procedure TCustomDBInformation.DataSetScrolled(Distance: Integer);
Begin
  {Gourmand en ressource}
End;

procedure TCustomDBInformation.DatasetChanged;
begin
  ActiveChanged;
end;

procedure TCustomDBInformation.CheckBrowseMode;
Begin
  ActiveChanged;
End;

procedure TCustomDBInformation.EditingChanged;
Begin
  ActiveChanged;
End;

procedure TCustomDBInformation.ActiveChanged;
begin
  If Enabled and FDataLink.Active then
  	ChangeCaption
  Else
  	ClearCaption;
end;


{ TDBRecordCount }

Constructor TDBRecordCount.Create(AOwner : TComponent);
Begin
	Inherited Create(AOwner);
  FRecordName:=msgDefaultRecordName;
  FPluralLetter:='s';
End;

Procedure TDBRecordCount.ChangeCaption;
Begin
  If (FDataLink.DataSet<>Nil) and (FDataLink.Active) then
  Begin
    If (FDataLink.DataSet.State=dsBrowse) then
    Begin
      Caption:=
        AutomaticPlural(FDataLink.DataSet.RecordCount,RecordName,FPluralLetter)
      {IntToStr(FDataLink.DataSet.RecordCount)+' '+RecordName;}
    End;
  End
  Else
    Caption:='';
End;

procedure TDBRecordCount.SetRecordName(const Value: string);
begin
  FRecordName:= Value;
  ChangeCaption;
end;

procedure TDBRecordCount.SetPluralLetter(const Value: string);
begin
  FPluralLetter:= Value;
  ChangeCaption;
end;

procedure TDBRecordCount.DatasetChanged;
begin
  {On inhibe pour ne pas réafficher inutilement le RecordCount}
  Inherited DatasetChanged;
end;


{ TDBState }

Procedure TDBState.ChangeCaption;
Var
  AState : TDataSetState;
Begin
  If (FDataLink.DataSet<>Nil) and (FDataLink.Active) then
  Begin
    AState:=FDataLink.Dataset.State;
    If (FState<>AState) then
    Begin
      FState:=AState;
      Case FState of
        dsInactive : Caption:=msgStateInactive;
        dsBrowse : Caption:=msgStateBrowse;
        dsEdit : Caption:=msgStateEdit;
        dsInsert : Caption:=msgStateInsert;
        dsSetKey : Caption:=msgStateSetKey;
        dsCalcFields : Caption:=msgStateCalcFields;
        {$IFDEF WIN32}
        dsFilter : Caption:=msgStateFilter;
        {$ENDIF}
  	  End;
    End;
  End
  Else
    Caption:='';
End;


{ TInfoDataLink }

constructor TInfoDataLink.Create(ADBInf: TCustomDBInformation);
begin
  inherited Create;
  FDBInformation := ADBInf;
end;

destructor TInfoDataLink.Destroy;
begin
  FDBInformation := nil;
  inherited Destroy;
end;

procedure TInfoDataLink.DataSetScrolled(Distance: Integer);
Begin
  if FDBInformation <> nil then FDBInformation.DatasetScrolled(Distance);
End;


procedure TInfoDataLink.EditingChanged;
begin
  if FDBInformation <> nil then FDBInformation.EditingChanged;
end;

procedure TInfoDataLink.DataSetChanged;
begin
  if FDBInformation <> nil then FDBInformation.DatasetChanged;
end;

procedure TInfoDataLink.ActiveChanged;
begin
  if FDBInformation <> nil then FDBInformation.ActiveChanged;
end;

procedure TInfoDataLink.CheckBrowseMode;
Begin
  if FDBInformation <> nil then FDBInformation.CheckBrowseMode;
End;

procedure Register;
begin
  RegisterComponents(SGrizzlyDB, [TDBRecordCount]);
  RegisterComponents(SGrizzlyDB, [TDBState]);
end;

end.
