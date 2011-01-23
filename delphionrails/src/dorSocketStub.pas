(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@gmail.com>.
*)

unit dorSocketStub;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  {$IFNDEF FPC}Windows,{$ENDIF}
  {$IFDEF FPC}sockets,{$ELSE}Winsock,{$ENDIF}
  {$IFDEF UNIX}baseunix,{$ENDIF}
  Generics.Collections,
  dorUtils, superobject;

{$IFDEF FPC}
const
  SOL_SOCKET    = $ffff;
  SO_REUSEADDR  = $0004;
  SO_RCVTIMEO   = $1006; 
  IPPROTO_TCP   = 6;
  TCP_NODELAY   = $0001;
{$ENDIF}
  
type
  // forward declarations
  TDORThread = class;
  TSocketStub = class;
  TSocketServer = class;

  TSocketStubClass = class of TSocketStub;
  TAbstractServerClass = class of TAbstractServer;
  TDORThreadClass = class of TDORThread;

  PThreadList = ^TThreadList;
  TThreadList = array[0..(Maxint div 16) - 1] of TDORThread;

  TDORThread = class(TInterfacedObject)
  private
    FThreadId: TThreadID;
    FThreadHandle: TThreadId;
    FPaused: boolean;
    FCriticalSection: TRtlCriticalSection;
    FOwner: TDORThread;
    FChildList: PThreadList;
    FChildCount: Integer;
    FChildCapacity: Integer;
    FThreadRefCount: Integer;
    FStopped: Longint;
    function ChildGet(Index: Integer): TDORThread;
    procedure ChildSetCapacity(NewCapacity: Integer);
    function ChildAdd(Item: TDORThread): Integer;
    procedure ChildDelete(Index: Integer);
    function ChildIndexOf(Item: TDORThread): Integer;
    function ChildRemove(Item: TDORThread): Integer;
    function GetChildCount: Integer;
    function GetStopped: boolean;
  protected
    function Run: Cardinal; virtual;
    procedure Stop; virtual;
  public
    class function ThreadCount: integer;
    property Owner: TDORThread read FOwner;
    procedure ChildClear; virtual;
    procedure Pause;
    procedure Resume;
    procedure Start;
    procedure Lock;
    procedure UnLock;
    constructor Create(AOwner: TDORThread); virtual;
    destructor Destroy; override;
    property ChildCount: Integer read GetChildCount;
    property ChildItems[Index: Integer]: TDORThread read ChildGet; default;
    property Stopped: boolean read GetStopped;
  end;

  TCustomObserver = class(TDORThread)
  private
    type
      TEventStorage = class
      private
        FEvents: ISOCriticalObject;
        FIntercept: ISOCriticalObject;
        FObservers: ISOCriticalObject;
        function Empty: ISuperObject;
      public
        constructor Create; virtual;
        procedure Trigger(const Event: ISuperObject);
      end;

      TEventProcessor = class(TDORThread)
      protected
        function Run: Cardinal; override;
      end;
    class var
      EventStorage: TEventStorage;
  public
    type
      TEventProc = reference to procedure(const event: ISuperObject);
  private
    FIntercepting: Boolean;
    FEvents: ISOCriticalObject;
    FEventProc: TDictionary<string, TEventProc>;
  protected
    procedure Intercept;
    procedure doOnEvent(const Event: ISuperObject); virtual;
    procedure doOnInternalEvent(const Event: ISuperObject); virtual;
    procedure ProcessEvents; virtual;
    function ExtractEvents: ISuperObject; virtual;
  public
    procedure RegisterEvent(const name: string; proc: TEventProc = nil); virtual;
    procedure UnregisterEvent(const name: string); virtual;
    procedure UnregisterEvents; virtual;
    constructor Create(AOwner: TDORThread); override;
    destructor Destroy; override;
    class procedure TriggerEvent(const Event: ISuperObject); virtual;
    procedure TriggerInternalEvent(const Event: ISuperObject); virtual;
    class constructor Create;
    class destructor Destroy;
  end;

  TAbstractServer = class(TDORThread)
  private
    FAddress: TSockAddr;
    FSocketHandle: LongInt;
    FPort: Word;
    FBind: Longint;
  public
    property Address: TSockAddr read FAddress;
    property SocketHandle: LongInt read FSocketHandle;
    constructor CreateServer(AOwner: TDORThread; Port: Word; const Bind: LongInt = INADDR_ANY); virtual;
  end;

  TSocketServer = class(TAbstractServer)
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
    function doOnCreateStub(Socket: longint; Address: TSockAddr): TSocketStub; virtual; abstract;
  end;

  TUDPServer = class(TAbstractServer)
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
  end;

  TSocketStub = class(TDORThread)
  private
    FAddress: TSockAddr;
    FSocketHandle: longint;
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
    procedure Release;
  public
    property SocketHandle: longint read FSocketHandle;
    property Address: TSockAddr read FAddress;
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); virtual;
  end;

threadvar
  CurrentThread: TDORThread;

implementation
uses
  SysUtils, dorService;

var
  AThreadCount: Integer = 0;

{$IFDEF FPC}
const
  INVALID_HANDLE_VALUE = TThreadId(-1);
  INVALID_SOCKET = longint(-1);
{$ENDIF}

function ThreadRun(Thread: Pointer): PtrInt; {$IFNDEF FPC}stdcall;{$ENDIF}
begin
  CurrentThread := TDORThread(Thread);
  InterlockedIncrement(CurrentThread.FThreadRefCount);
  try
    result := CurrentThread.Run;
  finally
    if InterlockedDecrement(CurrentThread.FThreadRefCount) = 0 then
      CurrentThread.Free else
      if CurrentThread.FOwner <> nil then
        CurrentThread.FOwner.ChildRemove(CurrentThread);
  end;
end;

{ TDORThread }

constructor TDORThread.Create(AOwner: TDORThread);
begin
  inherited Create;
  InterlockedIncrement(AThreadCount);
{$IFDEF FPC}
  InitCriticalSection(FCriticalSection);
{$ELSE}
  InitializeCriticalSection(FCriticalSection);
{$ENDIF}
  FPaused := False;
  InterlockedExchange(FStopped, 0);
  FOwner := AOwner;
  FThreadHandle := INVALID_HANDLE_VALUE;
  FThreadId := INVALID_HANDLE_VALUE;
  if (FOwner <> nil) then
    FOwner.ChildAdd(Self);
end;

destructor TDORThread.Destroy;
begin
  InterlockedDecrement(AThreadCount);
  Stop;
  ChildClear;
  //TerminateThread(FThreadHandle, 0);
{$IFDEF FPC}
  DoneCriticalSection(FCriticalSection);
{$ELSE}
  DeleteCriticalSection(FCriticalSection);
  if FThreadHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FThreadHandle);
{$ENDIF}
  inherited;
end;

procedure TDORThread.Pause;
var i: integer;
begin
  if not FPaused then
  begin
    Lock;
    try
      if FThreadHandle <> INVALID_HANDLE_VALUE then
        SuspendThread(FThreadHandle);
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Pause;
      FPaused := True;
    finally
      UnLock;
    end;
  end;
end;

procedure TDORThread.Resume;
var i: integer;
begin
  if FPaused then
  begin
    lock;
    try
      if FThreadHandle <> INVALID_HANDLE_VALUE then
        ResumeThread(FThreadHandle);
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Resume;
      FPaused := False;
    finally
      UnLock;
    end;
  end;
end;

function TDORThread.Run: Cardinal;
begin
  Result := 0;
//  raise Exception.Create('not implemented');
end;

// Childs ...

function TDORThread.ChildAdd(Item: TDORThread): Integer;
var
  Delta: Integer;
begin
  Lock;
  try
    Result := FChildCount;
    if Result = FChildCapacity then
    begin
      if FChildCapacity > 64 then
        Delta := FChildCapacity div 4
      else
        if FChildCapacity > 8 then
          Delta := 16
        else
          Delta := 4;
      ChildSetCapacity(FChildCapacity + Delta);
    end;
    FChildList^[Result] := Item;
    InterlockedIncrement(TDORThread(Item).FThreadRefCount);
    Inc(FChildCount);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildClear;
begin
  Lock;
  try
    while FChildCount > 0 do
      ChildRemove(ChildGet(0));
    ChildSetCapacity(0);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildDelete(Index: Integer);
begin
  if (Index < 0) or (Index >= FChildCount) then exit;

  with ChildGet(Index) do
    if InterlockedDecrement(FThreadRefCount) = 0 then
      Free else
      Stop;

  Dec(FChildCount);
  if Index < FChildCount then
    System.Move(FChildList^[Index + 1], FChildList^[Index],
      (FChildCount - Index) * SizeOf(Pointer));
end;

function TDORThread.ChildGet(Index: Integer): TDORThread;
begin
  Result := nil;
  Lock;
  try
    if (Index < 0) or (Index >= FChildCount) then
      raise Exception.CreateFmt('List index out of bounds (%d)', [Index]);
    Result := FChildList^[Index];
  finally
    UnLock;
  end;
end;

function TDORThread.ChildIndexOf(Item: TDORThread): Integer;
begin
  Result := 0;
  while (Result < FChildCount) and (FChildList^[Result] <> Item) do
    Inc(Result);
  if Result = FChildCount then
    Result := -1;
end;

function TDORThread.ChildRemove(Item: TDORThread): Integer;
begin
  Lock;
  try
    Result := ChildIndexOf(Item);
    if Result >= 0 then
      ChildDelete(Result);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildSetCapacity(NewCapacity: Integer);
begin
  Lock;
  try
    if (NewCapacity < FChildCount) or (NewCapacity > (Maxint div 16)) then
      raise Exception.CreateFmt('List capacity out of bounds (%d)', [NewCapacity]);
    if NewCapacity <> FChildCapacity then
    begin
      ReallocMem(FChildList, NewCapacity * SizeOf(Pointer));
      FChildCapacity := NewCapacity;
    end;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TDORThread.UnLock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TDORThread.GetChildCount: Integer;
begin
  Lock;
  try
    Result := FChildCount;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Start;
var
  i: Integer;
begin
  Lock;
  try
    if ClassType <> TDORThread then
    {$IFDEF FPC}
      FThreadHandle := BeginThread(@ThreadRun, Self, FThreadId);
    {$ELSE}
      FThreadHandle := CreateThread(nil, 0, @ThreadRun, Self, 0, FThreadId);
    {$ENDIF}
    for i := 0 to ChildCount - 1 do
      ChildItems[i].Start;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Stop;
begin
  InterlockedExchange(FStopped, 1);
end;

function TDORThread.GetStopped: boolean;
begin
  Result := FStopped <> 0;
end;

class function TDORThread.ThreadCount: integer;
begin
  Result := AThreadCount;
end;

{ TCustomObserver }

procedure TCustomObserver.doOnInternalEvent(const Event: ISuperObject);
begin

end;

procedure TCustomObserver.doOnEvent(const Event: ISuperObject);
begin

end;

function TCustomObserver.ExtractEvents: ISuperObject;
begin
  Result := FEvents.Extract;
end;

procedure TCustomObserver.TriggerInternalEvent(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
    FEvents.AsArray.Add(Event);
  finally
    FEvents.Unlock;
  end;
end;

class procedure TCustomObserver.TriggerEvent(const Event: ISuperObject);
begin
  EventStorage.Trigger(Event);
end;

procedure TCustomObserver.UnregisterEvent(const name: string);
var
  l: ISuperObject;
  j: Integer;
begin
  with EventStorage.FObservers do
  begin
    Lock;
    try
      l := AsObject[name];
      if l <> nil then
      for j := 0 to l.AsArray.Length - 1 do
        if l.AsArray[j] = FEvents then
        begin
          l.AsArray.Delete(j);
          Break;
        end;
    finally
      Unlock;
    end;
    FEventProc.Remove(name);
  end;
end;

procedure TCustomObserver.UnregisterEvents;
var
  l: ISuperObject;
  j: Integer;
  name: string;
begin
  with EventStorage.FObservers do
  begin
    for name in FEventProc.Keys do
    begin
      Lock;
      try
        l := AsObject[name];
        if l <> nil then
        for j := 0 to l.AsArray.Length - 1 do
          if l.AsArray[j] = FEvents then
          begin
            l.AsArray.Delete(j);
            Break;
          end;
      finally
        Unlock;
      end;
    end;
    FEventProc.Clear;
  end;

  if FIntercepting then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        for j := 0 to AsArray.Length - 1 do
          if AsArray[j] = FEvents then
          begin
            AsArray.Delete(j);
            Break;
          end;
      finally
        Unlock;
      end;
    end;

end;

procedure TCustomObserver.ProcessEvents;
var
  Event: ISuperObject;
begin
  for Event in ExtractEvents do
    if ObjectIsType(Event, stObject) and ObjectIsType(Event.AsObject['event'], stString) then
      FEventProc[Event.S['event']](Event) else
      doOnInternalEvent(Event);
end;

procedure TCustomObserver.Intercept;
begin
  if not FIntercepting then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        AsArray.Add(FEvents);
      finally
        Unlock;
      end;
      FIntercepting := True;
    end;
end;

procedure TCustomObserver.RegisterEvent(const name: string; proc: TEventProc);
var
  l: ISuperObject;
begin
  with EventStorage.FObservers do
    begin
      Lock;
      try
        l := AsObject[name];
        if l = nil then
        begin
          l := TSuperObject.Create(stArray);
          AsObject[name] := l;
        end;
        if Assigned(proc) then
          FEventProc.AddOrSetValue(name, proc) else
          FEventProc.AddOrSetValue(name, procedure(const event: ISuperObject) begin doOnEvent(event) end);
        l.AsArray.Add(FEvents);
      finally
        Unlock;
      end;
    end;
end;

class constructor TCustomObserver.Create;
begin
  Application := TDORService.Create;
  EventStorage := TEventStorage.Create;
  Application.CreateThread(TEventProcessor);
end;

destructor TCustomObserver.Destroy;
begin
  UnregisterEvents;
  FEvents := nil;
  FEventProc.Free;
  inherited;
end;

constructor TCustomObserver.Create(AOwner: TDORThread);
begin
  inherited;
  FEventProc := TDictionary<string, TEventProc>.Create;
  FEvents := TSOCriticalObject.Create(stArray);
  FIntercepting := False;
end;

class destructor TCustomObserver.Destroy;
begin
  while TDORThread.ThreadCount > 0 do Sleep(200);
  EventStorage.Free;
end;

{ TCustomObserver.TEventStorage }

constructor TCustomObserver.TEventStorage.Create;
begin
  FEvents := TSOCriticalObject.Create(stArray);
  FObservers := TSOCriticalObject.Create(stObject);
  FIntercept := TSOCriticalObject.Create(stArray);
end;

function TCustomObserver.TEventStorage.Empty: ISuperObject;
begin
  FEvents.Lock;
  try
    if FEvents.AsArray.Length > 0 then
      Result := FEvents.Extract else
      Result := nil;
  finally
    FEvents.Unlock;
  end;
end;

procedure TCustomObserver.TEventStorage.Trigger(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
    FEvents.AsArray.Add(Event);
  finally
    FEvents.Unlock;
  end;
end;

{ TCustomObserver.TEventProcessor }

function TCustomObserver.TEventProcessor.Run: Cardinal;
var
  events, event, box: ISuperObject;
begin
  while not Stopped do
  begin
    events := EventStorage.Empty;
    if events <> nil then
      for event in events do
      begin
        EventStorage.FIntercept.Lock;
        try
          for box in EventStorage.FIntercept do
          begin
            ISOCriticalObject(box).Lock;
            try
              box.AsArray.Add(event.Clone);
            finally
              ISOCriticalObject(box).Unlock;
            end;
          end;
        finally
          EventStorage.FIntercept.Unlock;
        end;

        EventStorage.FObservers.Lock;
        try
          for box in EventStorage.FObservers.N[event.AsObject.S['event']] do
          begin
            ISOCriticalObject(box).Lock;
            try
              box.AsArray.Add(event.Clone);
            finally
              ISOCriticalObject(box).Unlock;
            end;
          end;
        finally
          EventStorage.FObservers.Unlock;
        end;
      end;
    sleep(1);
  end;
  Result := 0;
end;

{ TAbstractServer }

constructor TAbstractServer.CreateServer(AOwner: TDORThread; Port: Word;
  const Bind: Integer);
begin
  inherited Create(AOwner);
  FSocketHandle := INVALID_SOCKET;
  FPort := Port;
  FBind := Bind;
end;

{ TSocketServer }

function TSocketServer.Run: Cardinal;
var
  InputSocket: longint;
  InputAddress: TSockAddr;
  InputLen: Integer;
  Stub: TSocketStub;
  SO_True: Integer;
begin
  SO_True := -1;
  Result := 0;
{$IFDEF FPC}
  FSocketHandle := fpsocket(AF_INET, SOCK_STREAM, 0);
{$ELSE}
  FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
{$ENDIF}
  FAddress.sin_addr.s_addr := FBind;
  FAddress.sin_family := AF_INET;
  FAddress.sin_port := htons(FPort);

{$IFDEF FPC}
  fpsetsockopt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_True), SizeOf(SO_True));
  fpsetsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PChar(@SO_True), SizeOf(SO_True));
{$ELSE}
  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_True), SizeOf(SO_True));
{$ENDIF}

{$IFDEF FPC}
  if fpbind(FSocketHandle, @FAddress, SizeOf(FAddress)) <> 0 then
{$ELSE}
  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
{$ENDIF}
  begin
    Stop;
    raise Exception.Create('can''t bind.');
  end;
{$IFDEF FPC}
  if (fplisten(FSocketHandle, 15) <> 0) then
{$ELSE}
  if (listen(FSocketHandle, 15) <> 0) then
{$ENDIF}
  begin
    Stop;
    raise Exception.Create('can''t listen.');
  end;

  InputLen := SizeOf(InputAddress);
  while not Stopped do
  try
{$IFDEF FPC}
    InputSocket := fpaccept(FSocketHandle, @InputAddress, @InputLen);
{$ELSE}
    InputSocket := accept(FSocketHandle, @InputAddress, @InputLen);
{$ENDIF}
    if (InputSocket <> INVALID_SOCKET) then
    begin
      Stub := doOnCreateStub(InputSocket, InputAddress);
      if Stub <> nil then
        Stub.Start else
        closesocket(InputSocket);
    end;
  except
    // the server must continue to listen !
  end;
end;

procedure TSocketServer.Stop;
{$IFDEF UNIX}
var
  addr: TSockAddr;
  ASocket: longint;
{$ENDIF}
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    closesocket(FSocketHandle);
  {$IFDEF UNIX}
    // connect to itself to stop waiting
    ASocket := fpsocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    addr.sin_family := AF_INET;
    addr.sin_port := htons(FPort);
    addr.sin_addr.S_addr := $0100007F; // 127.0.0.1
    fpconnect(ASocket, @addr, sizeof(addr));
    closesocket(ASocket);
  {$ENDIF}
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
  end;
end;

{ TUDPServer }

function TUDPServer.Run: Cardinal;
begin
  Result := 0;
{$IFDEF FPC}
  FSocketHandle := fpsocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
{$ELSE}
  FSocketHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
{$ENDIF}
  FAddress.sin_addr.s_addr := FBind;
  FAddress.sin_family := AF_INET;
  FAddress.sin_port := htons(FPort);

{$IFDEF FPC}
  if fpbind(FSocketHandle, @FAddress, SizeOf(FAddress)) <> 0 then
{$ELSE}
  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
{$ENDIF}
  begin
    Stop;
    raise Exception.Create('can''t bind.');
  end;
end;

procedure TUDPServer.Stop;
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    closesocket(FSocketHandle);
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
  end;
end;

{ TSocketStub }

constructor TSocketStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited Create(AOwner);
  FSocketHandle := ASocket;
  FAddress := AAddress;
end;

function TSocketStub.Run: Cardinal;
begin
  // you must implement this method
  Result := 0;
end;

procedure TSocketStub.Stop;
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    CloseSocket(FSocketHandle);
    Release;
  end;
end;

procedure TSocketStub.Release;
begin
  InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
end;

{$IFNDEF FPC}
initialization
  IsMultiThread := true;
{$ENDIF}

end.