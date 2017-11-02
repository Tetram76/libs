{***************************************************************
 *
 * Unit Name: FGLog
 * Purpose  : Logs component to easily log anything in a file or
              a dataset.
 * Author   : Frederic GUILLIEN fguillien@grizzlydev.com
 * History  : Beta release 99-08-26
 *
 ****************************************************************}

unit FGLog;

interface

uses
  SysUtils, Classes, GzLog, Forms, DB;


{
CREATE TABLE Errors
	(
	[Date] datetime NOT NULL,
	Computer varchar(50) NULL,
	[User] varchar(50) NULL,
	Application varchar(255) NOT NULL,
	Contexte varchar(255) NULL,
	Description varchar(255) NULL
	)
}
type
  EDatasetLogException = class(Exception);

  TLogField = string;

  TDatasetLog = class(TCustomLog)
  private
    { Déclarations privées }
    FDataset: TDataset;
    FContexteField: TLogField;
    FDescriptionField: TLogField;
    FMomentField: TLogField;
    FComputerNameField: TLogField;
    FUserNameField: TLogField;
    FApplicationNameField: TLogField;
    procedure SetDataset(Value: TDataSet);
  protected
    { Déclarations protégées }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    procedure LogEvent(AContexte, ADescription: string); override;
    procedure Clear; override;
    constructor Create(AOwner: TComponent); override;
  published
    { Déclarations publiées }
    property Dataset: TDataset read FDataset write SetDataset;
    property MomentField: TLogField read FMomentField write FMomentField;
{$IFDEF WIN32}
    property ComputerNameField: TLogField read FComputerNameField write FComputerNameField;
    property UserNameField: TLogField read FUserNameField write FUserNameField;
{$ENDIF}
    property ApplicationNameField: TLogField read FApplicationNameField write FApplicationNameField;
    property ContexteField: TLogField read FContexteField write FContexteField;
    property DescriptionField: TLogField read FDescriptionField write FDescriptionField;
  end;

  TFileErrorLog = class(TFileLog)
  private
    { Déclarations privées }
    FOldAppException: TExceptionEvent;
  protected
    { Déclarations protégées }
    procedure AppException(Sender: TObject; E: Exception);
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure LogError(Sender: TObject; E: Exception);
  published
    { Déclarations publiées }
  end;

  TDatasetErrorLog = class(TDatasetLog)
  private
    { Déclarations privées }
    FOldAppException: TExceptionEvent;
  protected
    { Déclarations protégées }
    procedure AppException(Sender: TObject; E: Exception);
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure LogError(Sender: TObject; E: Exception);
  published
    { Déclarations publiées }
  end;

implementation

uses FGUtils;

constructor TDatasetLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDatasetLog.SetDataset(Value: TDataSet);
begin
  if Value <> FDataset then
  begin
    FDataset:= Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TDatasetLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
    (AComponent = FDataset) then
  begin
    FDataset:= nil;
  end;
end;

procedure TDatasetLog.LogEvent(AContexte, ADescription: string);
var
  AOuvert: Boolean;
  function RecordField(AFieldName: string; AValue: string): Boolean;
  var
    TheField: TField;
  begin
    try
      Result:= False;
      TheField:= Dataset.FindField(AFieldName);
      if Assigned(TheField) then
      begin
        TheField.AsString:= AValue;
        Result:= True;
      end;
    except
      Result:= False
    end;
  end;
begin
  if Dataset <> nil then
  begin
    if Dataset.Active then
      AOuvert:= False
    else
    begin
      AOuvert:= True;
      Dataset.Open;
    end;
    try
      Dataset.Append;
      try
        RecordField(MomentField, DateTimeToStr(Now));
{$IFDEF WIN32}
        RecordField(ComputerNameField, GetComputerName);
        RecordField(UserNameField, GetUserName);
{$ENDIF}
        RecordField(ApplicationNameField, Application.ExeName);
        RecordField(ContexteField, WashedString(AContexte));
        RecordField(DescriptionField, WashedString(ADescription));
        Dataset.Post;
      except
        Dataset.Cancel;
        raise;
      end
    finally
      if AOuvert then
        Dataset.Close;
    end;
  end;
end;

constructor TFileErrorLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName:= 'ERROR.LOG';
  {Sauvegarde du gestionnaire précédent}
  FOldAppException:= Application.OnException;
  {Récupération vers notre gestionnaire}
  Application.OnException:= AppException;
end;

destructor TFileErrorLog.Destroy;
begin
  {Restauration de l'ancien gestionnaire}
  Application.OnException:= FOldAppException;
  inherited Destroy;
end;

procedure TFileErrorLog.AppException(Sender: TObject; E: Exception);
begin
  LogError(Sender, E);
  {Relancer l'ancien gestionnaire}
  if Assigned(FOldAppException) then
    FOldAppException(Sender, E)
  else
    Application.ShowException(E);
end;

procedure TFileErrorLog.LogError(Sender: TObject; E: Exception);
begin
  if Sender is TComponent then
  begin
    if Assigned(TComponent(Sender).Owner) then
      LogEvent(TComponent(Sender).Name + ';Owner ' + TComponent(Sender).Owner.Name, E.ClassName + ': ' + E.Message)
    else
      LogEvent(TComponent(Sender).Name, E.ClassName + ': ' + E.Message)
  end
  else
    LogEvent(Sender.ClassName, E.Message);
end;

constructor TDatasetErrorLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Sauvegarde du gestionnaire précédent}
  FOldAppException:= Application.OnException;
  {Récupération vers notre gestionnaire}
  Application.OnException:= AppException;
end;

destructor TDatasetErrorLog.Destroy;
begin
  {Restauration de l'ancien gestionnaire}
  Application.OnException:= FOldAppException;
  inherited Destroy;
end;

procedure TDatasetErrorLog.AppException(Sender: TObject; E: Exception);
begin
  LogError(Sender, E);
  {Relancer l'ancien gestionnaire}
  if Assigned(FOldAppException) then
    FOldAppException(Sender, E)
  else
    Application.ShowException(E);
end;

procedure TDatasetErrorLog.LogError(Sender: TObject; E: Exception);
begin
  if Sender is TComponent then
  begin
    if Assigned(TComponent(Sender).Owner) then
      LogEvent(TComponent(Sender).Name + ';Owner ' + TComponent(Sender).Owner.Name, E.ClassName + ': ' + E.Message)
    else
      LogEvent(TComponent(Sender).Name, E.ClassName + ': ' + E.Message)
  end
  else
    LogEvent(Sender.ClassName, E.ClassName + ': ' + E.Message);
end;

procedure TDatasetLog.Clear;
var
  AOuvert: Boolean;
begin
  if Dataset <> nil then
  begin
    if Dataset.Active then
      AOuvert:= False
    else
    begin
      AOuvert:= True;
      Dataset.Open;
    end;
    try
      Dataset.First;
      while not Dataset.EOF do
        Dataset.Delete;
    finally
      if AOuvert then
        Dataset.Close;
    end;
  end;
end;

end.

