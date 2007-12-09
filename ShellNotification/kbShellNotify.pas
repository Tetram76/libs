unit kbShellNotify;

interface

uses Windows, Messages, ShlObj, SysUtils, Classes, kbsnPIDL;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{********************************************************
  Undocumented Windows Shell Notification API interfaces
 ********************************************************}

const
  SHCNF_ACCEPT_INTERRUPTS     = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS = $0002;
  SHCNF_NO_PROXY              = $8000;

type
  TNotifyRegister = packed record
    pidlPath:      PItemIDList;
    bWatchSubtree: BOOL;
  end;
  PNotifyRegister = ^TNotifyRegister;

  TTwoPIDLArray = packed record
    PIDL1: PItemIDList;
    PIDL2: PItemIDList;
  end;
  PTwoPIDLArray = ^TTwoPIDLArray;

  TDWORDItemID = packed record
    cb:      Word;
    dwItem1: DWORD;
    dwItem2: DWORD;
  end;
  PDWORDItemID = ^TDWORDItemID;

function SHChangeNotifyRegister(Window: HWND; Flags: DWORD; EventMask: ULONG; MessageID: UINT;
                                ItemCount: DWORD; var Items: TNotifyRegister): THandle; stdcall;
function SHChangeNotifyDeregister(Notification: THandle): BOOL; stdcall;
function SHChangeNotification_Lock(MemoryMap: THandle; ProcessID: DWORD; var PIDLs: PTwoPIDLArray; var EventID: ULONG): THandle; stdcall;
function SHChangeNotification_Unlock(Lock: THandle): BOOL; stdcall;



{********************************************************
                   Public Enumerated Types
 ********************************************************}

type
  TkbShellNotifyEventType = (kbsnAnyEvent, kbsnDiskEvent, kbsnGlobalEvent, kbsnAssociationChanged, kbsnAttributesChanged,
                             kbsnDriveAdded, kbsnDriveRemoved, kbsnExtendedEvent, kbsnFolderCreated, kbsnFolderDeleted,
                             kbsnFolderRenamed, kbsnFolderUpdated, kbsnFreespaceChanged, kbsnImageUpdated, kbsnItemCreated,
                             kbsnItemDeleted, kbsnItemRenamed, kbsnItemUpdated, kbsnMediaInserted, kbsnMediaRemoved,
                             kbsnNetworkDriveAdded, kbsnResourceShared, kbsnResourceUnshared, kbsnServerDisconnected);
  TkbInterruptOption      = (kbioAcceptInterrupts, kbioAcceptNonInterrupts);



{********************************************************
                      Public Set Types
 ********************************************************}

type
  TkbShellNotifyEventTypes = set of TkbShellNotifyEventType;
  TkbInterruptOptions      = set of TkbInterruptOption;



{********************************************************
                Public Procedural Types
 ********************************************************}
TkbShellNotifySimpleEvent  = procedure(Sender: TObject; IsInterrupt: Boolean) of object;
TkbShellNotifyIndexEvent   = procedure(Sender: TObject; Index: LongInt; IsInterrupt: Boolean) of object;
TkbShellNotifyGeneralEvent = procedure(Sender: TObject; PIDL: Pointer; Path: TFileName; IsInterrupt: Boolean) of object;
TkbShellNotifyRenameEvent  = procedure(Sender: TObject; OldPIDL: Pointer; OldPath: TFileName; NewPIDL: Pointer;
                                       NewPath: TFileName; IsInterrupt: Boolean) of object;
TkbShellNotifyGenericEvent = procedure(Sender: TObject; EventType: TkbShellNotifyEventType; PIDL1: Pointer;
                                       PIDL2: Pointer; IsInterrupt: Boolean) of object;



{********************************************************
          TkbShellNotify class public interface
 ********************************************************}

type
  TkbShellNotify = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy;                      override;
  private
    FActive:           Boolean;
    FHandledEvents:    TkbShellNotifyEventTypes;
    FHandle:           THandle;
    FInterruptOptions: TkbInterruptOptions;
    FMessageWindow:    HWND;
    FRootFolder:       TkbSpecialLocation;
    FRootPath:         TFileName;
    FRootPIDL:         PItemIDList;
    FWatchChildren:    Boolean;
  private
    FOnAnyEvent:           TkbShellNotifyGenericEvent;
    FOnDiskEvent:          TkbShellNotifyGenericEvent;
    FOnGlobalEvent:        TkbShellNotifyGenericEvent;
    FOnAssociationChanged: TkbShellNotifySimpleEvent;
    FOnAttributesChanged:  TkbShellNotifyGeneralEvent;
    FOnDriveAdded:         TkbShellNotifyGeneralEvent;
    FOnDriveRemoved:       TkbShellNotifyGeneralEvent;
    FOnExtendedEvent:      TkbShellNotifyGenericEvent;
    FOnFolderRenamed:      TkbShellNotifyRenameEvent;
    FOnFolderCreated:      TkbShellNotifyGeneralEvent;
    FOnFolderDeleted:      TkbShellNotifyGeneralEvent;
    FOnFolderUpdated:      TkbShellNotifyGeneralEvent;
    FOnFreespaceChanged:   TkbShellNotifyGeneralEvent;
    FOnImageUpdated:       TkbShellNotifyIndexEvent;
    FOnItemCreated:        TkbShellNotifyGeneralEvent;
    FOnItemDeleted:        TkbShellNotifyGeneralEvent;
    FOnItemRenamed:        TkbShellNotifyRenameEvent;
    FOnItemUpdated:        TkbShellNotifyGeneralEvent;
    FOnMediaInserted:      TkbShellNotifyGeneralEvent;
    FOnMediaRemoved:       TkbShellNotifyGeneralEvent;
    FOnNetworkDriveAdded:  TkbShellNotifyGeneralEvent;
    FOnResourceShared:     TkbShellNotifyGeneralEvent;
    FOnResourceUnshared:   TkbShellNotifyGeneralEvent;
    FOnServerDisconnected: TkbShellNotifyGeneralEvent;
  private
    procedure HandleMessage(var TheMessage: TMessage);
    procedure ProcessEvent(EventID: DWORD; PIDLs: PTwoPIDLArray);
    procedure StartWatching;
    procedure StopWatching;
  protected
    procedure SetActive(NewValue: Boolean);
    procedure SetHandledEvents(NewValue: TkbShellNotifyEventTypes);
    procedure SetInterruptOptions(NewValue: TkbInterruptOptions);
    procedure SetWatchChildren(NewValue: Boolean);
    procedure SetRootFolder(NewValue: TkbSpecialLocation);
    procedure SetRootPath(NewValue: TFileName);
  protected
    procedure AnyEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
    procedure DiskEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
    procedure GlobalEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
    procedure AssociationChanged(IsInterrupt: Boolean);
    procedure AttributesChanged(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure DriveAdded(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure DriveRemoved(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ExtendedEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
    procedure FolderCreated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure FolderDeleted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure FolderRenamed(OldPIDL: PItemIDList; OldPath: TFileName; NewPIDL: PItemIDList; NewPath: TFileName; IsInterrupt: Boolean);
    procedure FolderUpdated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure FreespaceChanged(PIDL: PItemIDList; IsInterrupt: Boolean);
    procedure ImageUpdated(PIDL: PItemIDList; IsInterrupt: Boolean);
    procedure ItemCreated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ItemDeleted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ItemRenamed(OldPIDL: PItemIDList; OldPath: TFileName; NewPIDL: PItemIDList; NewPath: TFileName; IsInterrupt: Boolean);
    procedure ItemUpdated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure MediaInserted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure MediaRemoved(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure NetworkDriveAdded(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ResourceShared(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ResourceUnshared(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
    procedure ServerDisconnected(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
  public
    procedure Activate;
    procedure Deactivate;
    procedure Reset;
  public
    property Handle:           THandle                  read FHandle;
    property RootPIDL:         PItemIDList              read FRootPIDL;
  published
    property Active:           Boolean                  read FActive           write SetActive           default False;
    property HandledEvents:    TkbShellNotifyEventTypes read FHandledEvents    write SetHandledEvents    default [kbsnAnyEvent];
    property InterruptOptions: TkbInterruptOptions      read FInterruptOptions write SetInterruptOptions default [kbioAcceptInterrupts, kbioAcceptNonInterrupts];
    property RootFolder:       TkbSpecialLocation       read FRootFolder       write SetRootFolder;
    property RootPath:         TFileName                read FRootPath         write SetRootPath;
    property WatchChildren:    Boolean                  read FWatchChildren    write SetWatchChildren    default True;
  published
    property OnAnyEvent:           TkbShellNotifyGenericEvent read FOnAnyEvent           write FOnAnyEvent;
    property OnDiskEvent:          TkbShellNotifyGenericEvent read FOnDiskEvent          write FOnDiskEvent;
    property OnGlobalEvent:        TkbShellNotifyGenericEvent read FOnGlobalEvent        write FOnGlobalEvent;
    property OnAssociationChanged: TkbShellNotifySimpleEvent  read FOnAssociationChanged write FOnAssociationChanged;
    property OnAttributesChanged:  TkbShellNotifyGeneralEvent read FOnAttributesChanged  write FOnAttributesChanged;
    property OnDriveAdded:         TkbShellNotifyGeneralEvent read FOnDriveAdded         write FOnDriveAdded;
    property OnDriveRemoved:       TkbShellNotifyGeneralEvent read FOnDriveRemoved       write FOnDriveRemoved;
    property OnExtendedEvent:      TkbShellNotifyGenericEvent read FOnExtendedEvent      write FOnExtendedEvent;
    property OnFolderCreated:      TkbShellNotifyGeneralEvent read FOnFolderCreated      write FOnFolderCreated;
    property OnFolderDeleted:      TkbShellNotifyGeneralEvent read FOnFolderDeleted      write FOnFolderDeleted;
    property OnFolderRenamed:      TkbShellNotifyRenameEvent  read FOnFolderRenamed      write FOnFolderRenamed;
    property OnFolderUpdated:      TkbShellNotifyGeneralEvent read FOnFolderUpdated      write FOnFolderUpdated;
    property OnFreespaceChanged:   TkbShellNotifyGeneralEvent read FOnFreespaceChanged   write FOnFreespaceChanged;
    property OnImageUpdated:       TkbShellNotifyIndexEvent   read FOnImageUpdated       write FOnImageUpdated;
    property OnItemCreated:        TkbShellNotifyGeneralEvent read FOnItemCreated        write FOnItemCreated;
    property OnItemDeleted:        TkbShellNotifyGeneralEvent read FOnItemDeleted        write FOnItemDeleted;
    property OnItemRenamed:        TkbShellNotifyRenameEvent  read FOnItemRenamed        write FOnItemRenamed;
    property OnItemUpdated:        TkbShellNotifyGeneralEvent read FOnItemUpdated        write FOnItemUpdated;
    property OnMediaInserted:      TkbShellNotifyGeneralEvent read FOnMediaInserted      write FOnMediaInserted;
    property OnMediaRemoved:       TkbShellNotifyGeneralEvent read FOnMediaRemoved       write FOnMediaRemoved;
    property OnNetworkDriveAdded:  TkbShellNotifyGeneralEvent read FOnNetworkDriveAdded  write FOnNetworkDriveAdded;
    property OnResourceShared:     TkbShellNotifyGeneralEvent read FOnResourceShared     write FOnResourceShared;
    property OnResourceUnshared:   TkbShellNotifyGeneralEvent read FOnResourceUnshared   write FOnResourceUnshared;
    property OnServerDisconnected: TkbShellNotifyGeneralEvent read FOnServerDisconnected write FOnServerDisconnected;
  end;


{********************************************************
                Public unit method interfaces
 ********************************************************}

function ShellNotifyEnumToConst(NotifyType: TkbShellNotifyEventType): DWORD;
function ShellNotifyConstToEnum(NotifyType: DWORD): TkbShellNotifyEventType;


procedure Register;

implementation

uses Forms;

procedure Register;
begin
  RegisterComponents('Tetram', [TkbShellNotify]);
end;

{********************************************************
                   Private unit constants
 ********************************************************}

const
  WM_SHELLNOTIFY = WM_USER;

  Shell32                           = 'shell32.dll';
  SHChangeNotifyRegister_Index      = 2;
  SHChangeNotifyDeregister_Index    = 4;
  SHChangeNotification_Lock_Index   = 644;
  SHChangeNotification_Unlock_Index = 645;



{***********************************************************
 Undocumented Windows Shell Notification API implementations
 ***********************************************************}

function SHChangeNotifyRegister;   external Shell32  index SHChangeNotifyRegister_Index;
function SHChangeNotifyDeregister; external Shell32  index SHChangeNotifyDeregister_Index;


{This function is only supported on NT, and so must be dynamically loaded.}
function SHChangeNotification_Lock(MemoryMap: THandle; ProcessID: DWORD; var PIDLs: PTwoPIDLArray;
                                   var EventID: ULONG): THandle; stdcall;
type
  TheFunctionType = function(MemoryMap: THandle; ProcessID: DWORD; var PIDLs: PTwoPIDLArray;
                             var EventID: ULONG): THandle; stdcall;
var
  ShellDLL:    HMODULE;
  TheFunction: TheFunctionType;
begin
  Result := 0;
  if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    ShellDLL := LoadLibrary(PChar(Shell32));
    TheFunction := GetProcAddress(ShellDLL, PChar(SHChangeNotification_Lock_Index));
    if (Assigned(TheFunction)) then begin
      Result := TheFunction(MemoryMap, ProcessID, PIDLs, EventID);
    end;
  end; {if}
end;


{This function is only supported on NT, and so must be dynamically loaded.}
function SHChangeNotification_Unlock(Lock: THandle): BOOL; stdcall;
type
  TheFunctionType = function(Lock: THandle): BOOL; stdcall;
var
  ShellDLL:    HMODULE;
  TheFunction: TheFunctionType;
begin
  Result := False;
  if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    ShellDLL := LoadLibrary(PChar(Shell32));
    TheFunction := GetProcAddress(ShellDLL, PChar(SHChangeNotification_Unlock_Index));
    if (Assigned(TheFunction)) then begin
      Result := TheFunction(Lock);
    end;
  end; {if}
end;



{********************************************************
          TkbShellNotify class implementation
 ********************************************************}

constructor TkbShellNotify.Create(TheOwner: TComponent);
begin
  {Call ancestor constructor.}
  inherited Create(TheOwner);

  {Initialize simple-type data members.}
  Self.FActive           := False;
  Self.FHandle           := 0;
  Self.FHandledEvents    := [kbsnAnyEvent];
  Self.FInterruptOptions := [kbioAcceptInterrupts, kbioAcceptNonInterrupts];
  Self.FRootPath         := EmptyStr;
  Self.FRootPIDL         := nil;
  Self.FWatchChildren    := True;

  {Allocate a message-handling window.}
  Self.FMessageWindow :=  AllocateHWnd(Self.HandleMessage);

  {Force initialization of the Root Folder using the SetRootFolder procedure.}
  Self.SetRootFolder(kbslDesktop);
end;


destructor TkbShellNotify.Destroy;
begin
  {Shut off notification.}
  Self.StopWatching;

  {Free windows resources.}
  DeallocateHWnd(Self.FMessageWindow);

  {Call ancestor destructor.}
  inherited Destroy;
end;


procedure TkbShellNotify.HandleMessage(var TheMessage: TMessage);
var
  PIDLs:   PTwoPIDLArray;
  EventId: DWORD;
  Lock:    THandle;
begin
  {Handle only the WM_SHELLNOTIFY message.}
  if (TheMessage.Msg = WM_SHELLNOTIFY) then begin

    {If this is NT, use the memory map to access the PIDL data.}
    if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then begin
      Lock := SHChangeNotification_Lock(THandle(TheMessage.wParam),
                                        DWORD(TheMessage.lParam),
                                        PIDLs, EventId);
      if (Lock <> 0) then begin
        try {..finally}
          Self.ProcessEvent(EventId, PIDLs);
        finally
          SHChangeNotification_Unlock(Lock);
        end; {try..finally}
      end;  {if}
    end  {if}

    {If this is not NT, access the PIDL data directly.}
    else begin
      EventId := DWORD(TheMessage.lParam);
      PIDLs   := PTwoPIDLArray(TheMessage.wParam);
      Self.ProcessEvent(EventID, PIDLs);
    end; {else}
  end  {if}

  {Call the default windows proc for any other message.}
  else begin
    TheMessage.Result := DefWindowProc(Self.FMessageWindow, TheMessage.Msg, TheMessage.wParam, TheMessage.lParam);
  end;
end;


procedure TkbShellNotify.ProcessEvent(EventID: DWORD; PIDLs: PTwoPIDLArray);
var
  EventType:   TkbShellNotifyEventType;
  PIDL1:       PItemIDList;
  PIDL2:       PItemIDList;
  Path1:       TFileName;
  Path2:       TFileName;
  IsInterrupt: Boolean;
begin
  {Crack open the PIDL array.}
  PIDL1 := PIDLs.PIDL1;
  PIDL2 := PIDLs.PIDL2;

  {Try to convert PIDLs to Paths}
  Path1 := GetPathFromPIDL(PIDL1);
  Path2 := GetPathFromPIDL(PIDL2);

  {Determine if event is interrupt-caused.}
  IsInterrupt := Boolean(EventID and SHCNE_INTERRUPT);

  {Iterate through possible events and fire as appropriate.  This is necessary because
   event IDs are flags, and there may be more than one in a particular message.}
  for EventType := Low(TkbShellNotifyEventType) to High(TkbShellNotifyEventType) do begin

    {Skip the "multi" event types.}
    if (EventType in [kbsnAnyEvent, kbsnDiskEvent, kbsnGlobalEvent]) then begin
      Continue;
    end; {if}

    {If the current event type is flagged...}
    if ((ShellNotifyEnumToConst(EventType) and EventID) <> 0) then begin

      {Fire appropriate "multi" events for this specific event.}
      Self.AnyEvent(EventType, PIDL1, PIDL2, IsInterrupt);
      if ((ShellNotifyEnumToConst(kbsnGlobalEvent) and ShellNotifyEnumToConst(EventType)) <> 0) then begin
        Self.GlobalEvent(EventType, PIDL1, PIDL2, IsInterrupt);
      end; {if}
      if ((ShellNotifyEnumToConst(kbsnDiskEvent) and ShellNotifyEnumToConst(EventType)) <> 0) then begin
        Self.DiskEvent(EventType, PIDL1, PIDL2, IsInterrupt);
      end; {if}

      {Fire specific event.}
      case (EventType) of
        kbsnAssociationChanged: Self.AssociationChanged(IsInterrupt);
        kbsnAttributesChanged:  Self.AttributesChanged(PIDL1, Path1, IsInterrupt);
        kbsnDriveAdded:         Self.DriveAdded(PIDL1, Path1, IsInterrupt);
        kbsnDriveRemoved:       Self.DriveRemoved(PIDL1, Path1, IsInterrupt);
        kbsnExtendedEvent:      Self.ExtendedEvent(EventType, PIDL1, PIDL2, IsInterrupt);
        kbsnFolderCreated:      Self.FolderCreated(PIDL1, Path1, IsInterrupt);
        kbsnFolderDeleted:      Self.FolderDeleted(PIDL1, Path1, IsInterrupt);
        kbsnFolderRenamed:      Self.FolderRenamed(PIDL1, Path1, PIDL2, Path2, IsInterrupt);
        kbsnFolderUpdated:      Self.FolderUpdated(PIDL1, Path1, IsInterrupt);
        kbsnFreespaceChanged:   Self.FreespaceChanged(PIDL1, IsInterrupt);
        kbsnImageUpdated:       Self.ImageUpdated(PIDL1, IsInterrupt);
        kbsnItemCreated:        Self.ItemCreated(PIDL1, Path1, IsInterrupt);
        kbsnItemDeleted:        Self.ItemDeleted(PIDL1, Path1, IsInterrupt);
        kbsnItemRenamed:        Self.ItemRenamed(PIDL1, Path1, PIDL2, Path2, IsInterrupt);
        kbsnItemUpdated:        Self.ItemUpdated(PIDL1, Path1, IsInterrupt);
        kbsnMediaInserted:      Self.MediaInserted(PIDL1, Path1, IsInterrupt);
        kbsnMediaRemoved:       Self.MediaRemoved(PIDL1, Path1, IsInterrupt);
        kbsnNetworkDriveAdded:  Self.NetworkDriveAdded(PIDL1, Path1, IsInterrupt);
        kbsnResourceShared:     Self.ResourceShared(PIDL1, Path1, IsInterrupt);
        kbsnResourceUnshared:   Self.ResourceUnshared(PIDL1, Path1, IsInterrupt);
        kbsnServerDisconnected: Self.ServerDisconnected(PIDL1, Path1, IsInterrupt);
      end; {case}
    end; {if}
  end; {for}
end;


procedure TkbShellNotify.StartWatching;
var
  NotifyPathData: TNotifyRegister;
  Flags:          DWORD;
  EventType:      TkbShellNotifyEventType;
  EventMask:      DWORD;
begin
  {Initialize Flags.}
  Flags := SHCNF_NO_PROXY;
  if (kbioAcceptInterrupts in Self.InterruptOptions) then begin
    Flags := Flags or SHCNF_ACCEPT_INTERRUPTS;
  end; {if}
  if (kbioAcceptNonInterrupts in Self.InterruptOptions) then begin
    Flags := Flags or SHCNF_ACCEPT_NON_INTERRUPTS;
  end; {if}

  {Initialize EventMask.}
  EventMask := 0;
  for EventType := Low(TkbShellNotifyEventType) to High(TkbShellNotifyEventType) do begin
    if (EventType in Self.HandledEvents) then begin
      EventMask := EventMask or ShellNotifyEnumToConst(EventType);
    end; {if}
  end;

  {Initialize Notification Path data.}
  NotifyPathData.pidlPath      := Self.RootPIDL;
  NotifyPathData.bWatchSubtree := Self.WatchChildren;

  {Register for notification and store the handle.}
  Self.FHandle := SHChangeNotifyRegister(Self.FMessageWindow, Flags, EventMask, WM_SHELLNOTIFY, 1, NotifyPathData);

  {If registration failed, set Active to False.}
  if (Self.Handle = 0) then begin
    Self.Deactivate;
  end; {if}
end;


procedure TkbShellNotify.StopWatching;
begin
  {Deregister the notification handle and set the handle to nil.}
  SHChangeNotifyDeregister(Self.FHandle);
  Self.FHandle := 0;
end;


procedure TkbShellNotify.SetActive(NewValue: Boolean);
begin
  {Don't do anything if the new value is the same as the old.}
  if (NewValue <> Self.FActive) then begin
    {If we're activating, start watching. If we're deactivating, stop watching.}
    if (NewValue) then begin
      Self.StartWatching;
    end  {if}
    else begin
      Self.StopWatching;
    end; {else}

    {Update the data member.}
    Self.FActive := NewValue;
  end; {if}
end;


procedure TkbShellNotify.SetHandledEvents(NewValue: TkbShellNotifyEventTypes);
begin
  {Update the data member and reset to reflect the change.}
  if (NewValue <> Self.FHandledEvents) then begin
    Self.FHandledEvents := NewValue;
    Self.Reset;
  end; {if}
end;


procedure TkbShellNotify.SetInterruptOptions(NewValue: TkbInterruptOptions);
begin
  {Update the data member and reset to reflect the change.}
  if (NewValue <> Self.FInterruptOptions) then begin
    Self.FInterruptOptions := NewValue;
    Self.Reset;
  end; {if}
end;


procedure TkbShellNotify.SetRootFolder(NewValue: TkbSpecialLocation);
begin
  {Free any existing PIDL.}
  FreePIDL(Self.FRootPIDL);

  {If the root node is a DOS path, convert the path to a PIDL.  If this fails, make the
   root node the My Computer node.  If the root node is not a path, get the PIDL to
   the specified special location.}
  if (NewValue = kbslPath) then begin
    Self.FRootPIDL := GetPIDLFromPath(Self.RootPath);
    if (Self.RootPIDL = nil) then begin
      SHGetSpecialFolderLocation(Application.Handle, CSIDL_DRIVES, Self.FRootPIDL);
    end; {if}
  end  {if}
  else begin
    SHGetSpecialFolderLocation(Application.Handle, SpecialLocationEnumToConst(NewValue), Self.FRootPIDL);
  end; {else}

  {If all else fails, get the PIDL to the Desktop.}
  if (Self.RootPIDL = nil) then begin
    SHGetSpecialFolderLocation(Application.Handle, CSIDL_DESKTOP, Self.FRootPIDL);
  end; {if}

  {Update the data member.}
  Self.FRootFolder := NewValue;

  {Reset to reflect changes.}
  Self.Reset;
end;


procedure TkbShellNotify.SetRootPath(NewValue: TFileName);
begin
  {Update the path value and set the root folder to be a path.}
  Self.FRootPath := NewValue;

  {Set the root folder to an explicit path.  This handles the necessary reset.}
  Self.RootFolder := kbslPath;
end;


procedure TkbShellNotify.SetWatchChildren(NewValue: Boolean);
begin
  {Update the data member and reset to reflect the change.}
  if (NewValue <> Self.FWatchChildren) then begin
    Self.FWatchChildren := NewValue;
    Self.Reset;
  end; {if}
end;


procedure TkbShellNotify.AnyEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnAnyEvent)) then begin
    Self.OnAnyEvent(Self, EventType, PIDL1, PIDL2, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.DiskEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnDiskEvent)) then begin
    Self.OnDiskEvent(Self, EventType, PIDL1, PIDL2, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.GlobalEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnGlobalEvent)) then begin
    Self.OnGlobalEvent(Self, EventType, PIDL1, PIDL2, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.AssociationChanged(IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnAssociationChanged)) then begin
    Self.OnAssociationChanged(Self, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.AttributesChanged(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnAttributesChanged)) then begin
    Self.OnAttributesChanged(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.DriveAdded(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnDriveAdded)) then begin
    Self.OnDriveAdded(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.DriveRemoved(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnDriveRemoved)) then begin
    Self.OnDriveRemoved(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ExtendedEvent(EventType: TkbShellNotifyEventType; PIDL1: PItemIDList; PIDL2: PItemIDList; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnExtendedEvent)) then begin
    Self.OnAnyEvent(Self, EventType, PIDL1, PIDL2, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.FolderCreated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnFolderCreated)) then begin
    Self.OnFolderCreated(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.FolderDeleted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnFolderDeleted)) then begin
    Self.OnFolderDeleted(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.FolderRenamed(OldPIDL: PItemIDList; OldPath: TFileName; NewPIDL: PItemIDList; NewPath: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnFolderRenamed)) then begin
    Self.OnFolderRenamed(Self, OldPIDL, OldPath, NewPIDL, NewPath, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.FolderUpdated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnFolderUpdated)) then begin
    Self.OnFolderUpdated(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.FreespaceChanged(PIDL: PItemIDList; IsInterrupt: Boolean);
const
  DriveDelimiter = ':\';
var
  Index:       Integer;
  DriveMap:    LongInt;
  DrivePath:   TFileName;
begin
  {Don't do anything if event handler is not assigned.}
  if (Assigned(Self.OnFreespaceChanged)) then begin
    {Extract the bitmap specifying the drives affected.}
    DriveMap := PDWORDItemID(PIDL)^.dwItem1;

    {Iterate through the bits and generate an event for each flagged drive.}
    for Index := 0 to 25 do begin
      if ((($00000001 shl Index) and DriveMap) <> 0) then begin
        DrivePath := Chr(Ord('A') + Index) + DriveDelimiter;
        Self.OnFreespaceChanged(Self, GetPIDLFromPath(DrivePath), DrivePath, IsInterrupt);
      end; {if}
    end; {for}
  end; {if}
end;


procedure TkbShellNotify.ImageUpdated(PIDL: PItemIDList; IsInterrupt: Boolean);
var
  Index: LongInt;
begin
  {If assigned, crack out the image index and call event handler.}
  if (Assigned(Self.OnImageUpdated)) then begin
    Index := PDWORDItemID(PIDL)^.dwItem1;
    Self.OnImageUpdated(Self, Index, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ItemCreated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnItemCreated)) then begin
    Self.OnItemCreated(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ItemDeleted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnItemDeleted)) then begin
    Self.OnItemDeleted(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ItemRenamed(OldPIDL: PItemIDList; OldPath: TFileName; NewPIDL: PItemIDList; NewPath: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnItemRenamed)) then begin
    Self.OnItemRenamed(Self, OldPIDL, OldPath, NewPIDL, NewPath, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ItemUpdated(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnItemUpdated)) then begin
    Self.OnItemUpdated(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.MediaInserted(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnMediaInserted)) then begin
    Self.OnMediaInserted(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.MediaRemoved(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnMediaRemoved)) then begin
    Self.OnMediaRemoved(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.NetworkDriveAdded(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnNetworkDriveAdded)) then begin
    Self.OnNetworkDriveAdded(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ResourceShared(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnResourceShared)) then begin
    Self.OnResourceShared(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ResourceUnshared(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnResourceUnshared)) then begin
    Self.OnResourceUnshared(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.ServerDisconnected(PIDL: PItemIDList; Path: TFileName; IsInterrupt: Boolean);
begin
  {Call event handler if assigned.}
  if (Assigned(Self.OnServerDisconnected)) then begin
    Self.OnServerDisconnected(Self, PIDL, Path, IsInterrupt);
  end; {if}
end;


procedure TkbShellNotify.Activate;
begin
  {Ensure the component is Active.}
  Self.Active := True;
end;


procedure TkbShellNotify.Deactivate;
begin
  {Ensure the component is not Active.}
  Self.Active := False;
end;


procedure TkbShellNotify.Reset;
begin
  {If the component is active, recycle the event watching mechanism.}
  if (Self.Active) then begin
    Self.StopWatching;
    Self.StartWatching;
  end; {if}
end;



{********************************************************
               Public unit method implementations
 ********************************************************}

function ShellNotifyEnumToConst(NotifyType: TkbShellNotifyEventType): DWORD;
begin
  case (NotifyType) of
    kbsnAnyEvent:           Result := SHCNE_ALLEVENTS;
    kbsnDiskEvent:          Result := SHCNE_DISKEVENTS;
    kbsnGlobalEvent:        Result := SHCNE_GLOBALEVENTS;
    kbsnAssociationChanged: Result := SHCNE_ASSOCCHANGED;
    kbsnAttributesChanged:  Result := SHCNE_ATTRIBUTES;
    kbsnDriveAdded:         Result := SHCNE_DRIVEADD;
    kbsnDriveRemoved:       Result := SHCNE_DRIVEREMOVED;
    kbsnExtendedEvent:      Result := SHCNE_EXTENDED_EVENT;
    kbsnItemCreated:        Result := SHCNE_CREATE;
    kbsnItemDeleted:        Result := SHCNE_DELETE;
    kbsnItemUpdated:        Result := SHCNE_UPDATEITEM;
    kbsnItemRenamed:        Result := SHCNE_RENAMEITEM;
    kbsnFolderCreated:      Result := SHCNE_MKDIR;
    kbsnFolderDeleted:      Result := SHCNE_RMDIR;
    kbsnFolderRenamed:      Result := SHCNE_RENAMEFOLDER;
    kbsnFolderUpdated:      Result := SHCNE_UPDATEDIR;
    kbsnFreespaceChanged:   Result := SHCNE_FREESPACE;
    kbsnImageUpdated:       Result := SHCNE_UPDATEIMAGE;
    kbsnMediaInserted:      Result := SHCNE_MEDIAINSERTED;
    kbsnMediaRemoved:       Result := SHCNE_MEDIAREMOVED;
    kbsnNetworkDriveAdded:  Result := SHCNE_DRIVEADDGUI;
    kbsnResourceShared:     Result := SHCNE_NETSHARE;
    kbsnResourceUnshared:   Result := SHCNE_NETUNSHARE;
    kbsnServerDisconnected: Result := SHCNE_SERVERDISCONNECT;
    else                    Result := 0;
  end; {case}
end;


function ShellNotifyConstToEnum(NotifyType: DWORD): TkbShellNotifyEventType;
begin
  case (NotifyType) of
    {Leave this one commented; it freaks out the Delphi 3 compiler.
     The ELSE takes care of it anyway.}
   {SHCNE_ALLEVENTS:        Result := kbsnAnyEvent;}
    SHCNE_DISKEVENTS:       Result := kbsnDiskEvent;
    SHCNE_GLOBALEVENTS:     Result := kbsnGlobalEvent;
    SHCNE_ASSOCCHANGED:     Result := kbsnAssociationChanged;
    SHCNE_ATTRIBUTES:       Result := kbsnAttributesChanged;
    SHCNE_DRIVEADD:         Result := kbsnDriveAdded;
    SHCNE_DRIVEREMOVED:     Result := kbsnDriveRemoved;
    SHCNE_EXTENDED_EVENT:   Result := kbsnExtendedEvent;
    SHCNE_FREESPACE:        Result := kbsnFreespaceChanged;
    SHCNE_MKDIR:            Result := kbsnFolderCreated;
    SHCNE_RMDIR:            Result := kbsnFolderDeleted;
    SHCNE_RENAMEFOLDER:     Result := kbsnFolderRenamed;
    SHCNE_UPDATEDIR:        Result := kbsnFolderUpdated;
    SHCNE_UPDATEIMAGE:      Result := kbsnImageUpdated;
    SHCNE_CREATE:           Result := kbsnItemCreated;
    SHCNE_DELETE:           Result := kbsnItemDeleted;
    SHCNE_RENAMEITEM:       Result := kbsnItemRenamed;
    SHCNE_UPDATEITEM:       Result := kbsnItemUpdated;
    SHCNE_MEDIAINSERTED:    Result := kbsnMediaInserted;
    SHCNE_MEDIAREMOVED:     Result := kbsnMediaRemoved;
    SHCNE_DRIVEADDGUI:      Result := kbsnNetworkDriveAdded;
    SHCNE_NETSHARE:         Result := kbsnResourceShared;
    SHCNE_NETUNSHARE:       Result := kbsnResourceUnshared;
    SHCNE_SERVERDISCONNECT: Result := kbsnServerDisconnected;
    else                    Result := kbsnAnyEvent;
  end; {case}
end;

end.
