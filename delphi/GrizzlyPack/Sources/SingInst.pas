unit SingInst;

interface

uses Classes, ASysUtil;

type
  TAppRunningEvent = procedure(Sender: TObject; HPrevInstance: Integer) of object;

  TSingleInstance = class(TComponent)
  private
    FFileMap: TFileMapStream;
    FIdentifier: string;
    FAppRunningEvent: TAppRunningEvent;
    function GetHandle: Integer;
    procedure SetHandle(const Value: Integer);
    procedure SetIdentifier(Value: string);
  protected
    procedure CreateAndCheck(var Identifier: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    {}
    class function IsAppRunning(AppId: string): Integer;
    {}
    property ActiveWindowHandle: Integer read GetHandle write SetHandle;
  published
    property AppIdentifier: string read FIdentifier write SetIdentifier;
    {}
    property OnAppRunning: TAppRunningEvent read FAppRunningEvent write FAppRunningEvent;
  end;

implementation

uses Windows, Forms, AUtils, SysUtils;

function TSingleInstance.GetHandle: Integer;
begin
  FFileMap.Seek(0, 0);
  FFileMap.Read(Result, SizeOf(Integer));
end;

procedure TSingleInstance.SetHandle(const Value: Integer);
begin
  FFileMap.Seek(0, 0);
  FFileMap.Write(Value, SizeOf(Integer));
end;

procedure TSingleInstance.SetIdentifier(Value: string);
begin
  CreateAndCheck(Value);
  FIdentifier:= Value;
end;

procedure TSingleInstance.CreateAndCheck(var Identifier: string);
var HPrevInstance: Integer;
begin
  FFileMap.Free; FFileMap:= nil;
  if not (csDesigning in ComponentState) then
  begin
    FFileMap:= TFileMapStream.Create(Identifier, SizeOf(Integer));
    if not FFileMap.HandleOwned then
    begin
      try
        FFileMap.Read(HPrevInstance, SizeOf(Integer));
        if Assigned(OnAppRunning) then
          OnAppRunning(Self, HPrevInstance);
        ShowWindow(HPrevInstance, SW_RESTORE);
        SetForeGroundWindow(HPrevInstance);
        Halt;
      finally
        FFileMap.Free; FFileMap:= nil;
      end;
    end else begin
      HPrevInstance:= Application.Handle;
      FFileMap.Write(HPrevInstance, SizeOf(Integer));
    end;
  end;
end;

constructor TSingleInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSingleInstance.Destroy;
begin
  FFileMap.Free;
  inherited Destroy;
end;

procedure TSingleInstance.Loaded;
var S: string;
begin
  if AppIdentifier = '' then
    S:= UpperCase(Application.ExeName)
  else
    S:= AppIdentifier;
  CreateAndCheck(S);
  inherited Loaded;
end;

{ Fonction pouvant être utilisées pour d'autres applis utilisant TSingleInstance }
class function TSingleInstance.IsAppRunning(AppId: string): Integer;
var FileMap: TFileMapStream;
begin
  FileMap:= TFileMapStream.Create(AppId, SizeOf(Integer));
  try
    if not FileMap.HandleOwned then
      FileMap.Read(Result, SizeOf(Integer))
    else
      Result:= 0;
  finally
    FileMap.Free;
  end;
end;

end.
