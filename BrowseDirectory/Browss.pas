unit Browss;
{$D-}
interface

uses
  Windows, ShlObj, Controls, Classes{, DsgnIntf};

const
     CSIDL_INTERNET         = $0001;
     CSIDL_DESKTOPEXPANDED  = $FEFE;
     BIF_BROWSEINCLUDEFILES = $4000;


type
  TRootID = (
    idDesktop, idInternet, idPrograms, idControlPanel, idPrinters, idPersonal,
    idFavorites, idStartup, idRecent, idSendTo, idRecycleBin, idStartMenu,
    idDesktopDirectory, idDrives, idNetwork, idNetHood, idFonts, idTemplates,
    idDesktopExpanded
   );

  TBrowseFlag = (
    bfDirectoriesOnly, bfDomainOnly, bfAncestors, bfComputers, bfPrinters, bfIncludeFiles
   );
  TBrowseFlags = set of TBrowseFlag;

  TBDSelChangedEvent = procedure(Sender: TObject; const NewSel: string) of object;

type
  TBrowseDirectoryDlg = class(TComponent)
  private
    FDlgWnd: HWND;
    FCaption: string;
    FParent: TWinControl;
    FShowSelectionInStatus: boolean;
    FFitStatusText: boolean;
    FTitle: string;
    FRoot: TRootID;
    FOptions: TBrowseFlags;
    FSelection: string;
    FCenter: boolean;
    FStatusText: string;
    FEnableOKButton: boolean;
    FImageIndex: integer;
    FSelChanged: TBDSelChangedEvent;
    FOnCreate: TNotifyEvent;
  protected
    function FittedStatusText: string;
    procedure DoInitialized(Wnd: HWND); virtual;
    procedure DoSelChanged(Wnd: HWND; Item: PItemIDList); virtual;
    procedure SetFitStatusText(Val: boolean);
    procedure SetStatusText(const Val: string);
    procedure SetSelection(const Val: string);
    procedure SetEnableOKButton(Val: boolean);
    function GetCaption: string;
    procedure SetCaption(const Val: string);
    procedure SetParent(AParent: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; virtual;
    property Parent: TWinControl read FParent write SetParent;
  published
    property Selection: string read FSelection write SetSelection;
    property Title: string read FTitle write FTitle;
    property Root: TRootID read FRoot write FRoot default idDesktop;
    property Options: TBrowseFlags read FOptions write FOptions default [];
    property Center: boolean read FCenter write FCenter default TRUE;
    property StatusText: string read FStatusText write SetStatusText;
    property FitStatusText: boolean read FFitStatusText write SetFitStatusText default TRUE;
    property EnableOKButton: boolean read FEnableOKButton write SetEnableOKButton default TRUE;
    property ImageIndex: integer read FImageIndex;
    property Caption: string read GetCaption write SetCaption;
    property ShowSelectionInStatus: boolean read FShowSelectionInStatus write FShowSelectionInStatus;
    property OnSelChanged: TBDSelChangedEvent read FSelChanged write FSelChanged;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
  end;

  { A component editor (not really) to allow on-the-fly testing of the      }
  { dialog.  Right click the component and select 'Test Dialog', or simply  }
  { double click the component, and the browse dialog will be displayed     }
  { with the current settings.                                              }
{  TBrowseDialogEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer): string; override;
    function GetVerbCount : Integer; override;
    procedure Edit; override;
  end;
}
{ Utility function you may find useful }
function DirExists(const Dir: string): boolean;

procedure Register;

implementation

uses Forms, Dialogs,

  ActiveX,
  SysUtils, Messages;


function ConvertRoot(Root: TRootID): integer;
const
  RootValues: array[TRootID] of integer = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS,
    CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO,
    CSIDL_BITBUCKET, CSIDL_STARTMENU, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK,
    CSIDL_NETHOOD, CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_DESKTOPEXPANDED
   );
begin
  Result := RootValues[Root];
end;

function ConvertFlags(Flags: TBrowseFlags): UINT;
const
  FlagValues: array[TBrowseFlag] of UINT = (
    BIF_RETURNONLYFSDIRS, BIF_DONTGOBELOWDOMAIN, BIF_RETURNFSANCESTORS,
    BIF_BROWSEFORCOMPUTER, BIF_BROWSEFORPRINTER, BIF_BROWSEINCLUDEFILES
   );
var
  Opt: TBrowseFlag;
begin
  Result := 0;
  { Loop through all possible values }
  for Opt := Low(TBrowseFlag) to High(TBrowseFlag) do
    if Opt in Flags then
      Result := Result OR FlagValues[Opt];
end;

function GetTextWidth(DC: HDC; const Text: String): Integer;
var
  Extent: TSize;
begin
  if GetTextExtentPoint(DC, PChar(Text), Length(Text), Extent) then
    Result := Extent.cX
  else
    Result := 0;
end;

function MinimizeName(Wnd: HWND; const Filename: string): string;

  procedure CutFirstDirectory(var S: string);
  var
    Root: Boolean;
    P: Integer;
  begin
    if S = '\' then
      S := ''
    else begin
      if S[1] = '\' then begin
        Root := True;
        Delete(S, 1, 1);
      end else
        Root := False;
      if S[1] = '.' then
        Delete(S, 1, 4);
      P := Pos('\',S);
      if P <> 0 then begin
        Delete(S, 1, P);
        S := '...\' + S;
      end else
        S := '';
      if Root then
        S := '\' + S;
    end;
  end;

var
  Drive: string;
  Dir: string;
  Name: string;
  R: TRect;
  DC: HDC;
  MaxLen: integer;
  OldFont, Font: HFONT;
begin
  Result := FileName;
  if Wnd = 0 then exit;
  DC := GetDC(Wnd);
  if DC = 0 then exit;
  Font := SendMessage(Wnd, WM_GETFONT, 0, 0);
  OldFont := SelectObject(DC, Font);
  try
    GetWindowRect(Wnd, R);
    MaxLen := R.Right - R.Left;

    Dir := ExtractFilePath(Result);
    Name := ExtractFileName(Result);

    if (Length(Dir) >= 2) and (Dir[2] = ':') then begin
      Drive := Copy(Dir, 1, 2);
      Delete(Dir, 1, 2);
    end else
      Drive := '';
    while ((Dir <> '') or (Drive <> '')) and (GetTextWidth(DC, Result) > MaxLen) do begin
      if Dir = '\...\' then begin
        Drive := '';
        Dir := '...\';
      end else if Dir = '' then
        Drive := ''
      else
        CutFirstDirectory(Dir);
      Result := Drive + Dir + Name;
    end;
  finally
    SelectObject(DC, OldFont);
    ReleaseDC(Wnd, DC);
  end;
end;

function MinimizeString(Wnd: HWND; const Text: string): string;
var
  R: TRect;
  DC: HDC;
  MaxLen: integer;
  OldFont, Font: HFONT;
  TempStr: string;
begin
  Result := Text;
  TempStr := Text;
  if Wnd = 0 then exit;
  DC := GetDC(Wnd);
  if DC = 0 then exit;
  Font := SendMessage(Wnd, WM_GETFONT, 0, 0);
  OldFont := SelectObject(DC, Font);
  try
    GetWindowRect(Wnd, R);
    MaxLen := R.Right - R.Left;
    while (TempStr <> '') and (GetTextWidth(DC, Result) > MaxLen) do begin
      SetLength(TempStr, Length(TempStr)-1);
      Result := TempStr + '...';
    end;
  finally
    SelectObject(DC, OldFont);
    ReleaseDC(Wnd, DC);
  end;
end;


function DirExists(const Dir: string): boolean;
  function StripTrailingBackslash(const Dir: string): string;
  begin
    Result := Dir;
    // Make sure we have a string, and if so, see if the last char is a \
    if (Result <> '') and (Result[Length(Result)] = '\') then
      SetLength(Result, Length(Result)-1); // Shorten the length by one to remove
  end;
var
  Tmp: string;
  DriveBits: set of 0..25;
  SR: TSearchRec;
begin
  if (Length(Dir) = 3) and (Dir[2] = ':') and (Dir[3] = '\') then begin
    Integer(DriveBits) := GetLogicalDrives;
    Tmp := UpperCase(Dir[1]);
    Result := (ord(Tmp[1]) - ord('A')) in DriveBits;
  end else begin
    Result := (FindFirst(StripTrailingBackslash(Dir), faDirectory, SR) = 0) and (Dir <> '');
    if Result then
      Result := (SR.Attr and faDirectory) = faDirectory;
    FindClose(SR);
  end;
end; // DirExists

function BrowseCallbackProc(Wnd: HWnd; Msg: UINT; lParam: LPARAM; lData: LPARAM): integer; stdcall;
begin
  Result := 0;
  case Msg of
    BFFM_INITIALIZED:
      if lData <> 0 then
        TBrowseDirectoryDlg(lData).DoInitialized(Wnd);
    BFFM_SELCHANGED:
      if lData <> 0 then
        TBrowseDirectoryDlg(lData).DoSelChanged(Wnd, PItemIDList(lParam));
  end;
end;


function BrowseDirectory(var Dest: string; var ImgIdx: integer;
                         const AParent: TWinControl; const Title: string; Root: TRootID;
                         Flags: TBrowseFlags; WantStatusText: boolean;
                         Callback: TFNBFFCallBack; Data: Longint): boolean;
var
  ShellMalloc: IMALLOC;
  shBuff: PChar;
  BrowseInfo: TBrowseInfo;
  idRoot, idBrowse: PItemIDList;
  WndHandle: HWND;
begin
  Result := FALSE; // Assume the worst.
  Dest := ''; // Clear it out.
  SetLength(Dest, MAX_PATH);  // Make sure their will be enough room in dest.
  if assigned(AParent) then
    WndHandle := AParent.Handle
  else
    WndHandle := 0;
  if SHGetMalloc(ShellMalloc) = NOERROR then begin
    try
      shBuff := PChar(ShellMalloc.Alloc(MAX_PATH)); // Shell allocate buffer.
      if assigned(shBuff) then begin
        try
          // Get id for desired root item.
          SHGetSpecialFolderLocation(WndHandle, ConvertRoot(Root), idRoot);
          try
            with BrowseInfo do begin  // Fill info structure
              hwndOwner := WndHandle;
              pidlRoot := idRoot;
              pszDisplayName := shBuff;
              lpszTitle := PChar(Title);
              ulFlags := ConvertFlags(Flags);
              if WantStatusText then
                ulFlags := ulFlags or BIF_STATUSTEXT;
              lpfn := Callback;
              lParam := Data;
            end;
            idBrowse := SHBrowseForFolder(BrowseInfo);
            if assigned(idBrowse) then begin
              try
                SHGetPathFromIDList(idBrowse, shBuff); // Turn into real path.
                Dest := shBuff; // Put it in user's variable.
                ImgIdx := BrowseInfo.iImage; // Update the image index.
                Result := TRUE; // Success!
              finally
                ShellMalloc.Free(idBrowse); // Clean up after ourselves
              end;
            end;
          finally
            ShellMalloc.Free(idRoot); // Clean-up.
          end;
        finally
          ShellMalloc.Free(shBuff); // Clean-up.
        end;
      end;
    finally
      ShellMalloc._Release; // Clean-up.
    end;
  end;
end;


constructor TBrowseDirectoryDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDlgWnd := 0;
  FFitStatusText := TRUE;
  FEnableOKButton := TRUE;
  FTitle := '';
  FRoot := idDesktop;
  FOptions := [];
  FSelection := '';
  FCenter := TRUE;
  FSelChanged := NIL;
  FStatusText := '';
  FImageIndex := -1;
  FCaption := '';

  if assigned(AOwner) then
    if AOwner is TWinControl then
      FParent := TWinControl(Owner)
    else if assigned(Application) and assigned(Application.MainForm) then
      FParent := Application.MainForm;
end;

destructor TBrowseDirectoryDlg.Destroy;
begin
  inherited Destroy;
end;

function TBrowseDirectoryDlg.Execute: boolean;
var
  S: string;
  AParent: TWinControl;
begin
  { Assume the worst }
  AParent := NIL;
  if not (csDesigning in ComponentState) then
    { Determine who the parent is. }
    if assigned(FParent) then
      AParent := FParent
    else begin
      if assigned(Owner) then
        if Owner is TWinControl then
          AParent := TWinControl(Owner)
        else
          if assigned(Application) and assigned(Application.MainForm) then
            AParent := Application.MainForm;
    end;

  { Call the function }
  Result := BrowseDirectory(S, FImageIndex, AParent, FTitle, FRoot, FOptions,
                            FStatusText <> '', BrowseCallbackProc, LongInt(Self));

  FDlgWnd := 0; { Not valid any more. }

  { If selection made, update property }
  if Result then
    Selection := S
  else
    Selection := ''
end;

function FormatSelection(const APath: string): string;
begin
  Result := APath;
  if Result <> '' then begin
    if (Length(Result) < 4) and (Result[2] = ':') then begin
      if Length(Result) = 2 then
        Result := Result + '\'
    end else
      if Result[Length(Result)] = '\' then
        SetLength(Result, Length(Result)-1);
  end;
end;

procedure TBrowseDirectoryDlg.DoInitialized(Wnd: HWND);
var
  Rect: TRect;
begin
  FDlgWnd := Wnd;
  if FCenter then begin
    GetWindowRect(Wnd, Rect);
    SetWindowPos(Wnd, 0,
      (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
      (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 2,
      0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
  end;
  // Documentation for BFFM_ENABLEOK is incorrect.  Value sent in LPARAM, not WPARAM.
  SendMessage(FDlgWnd, BFFM_ENABLEOK, 0, LPARAM(FEnableOKButton));
  if FStatusText <> '' then
    SendMessage(Wnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
  if FSelection <> '' then
    SendMessage(FDlgWnd, BFFM_SETSELECTION, 1, LPARAM(FormatSelection(FSelection)));
  if FCaption <> '' then
    SendMessage(FDlgWnd, WM_SETTEXT, 0, LPARAM(FCaption));
  if assigned(FOnCreate) then
    FOnCreate(Self);
end;

procedure TBrowseDirectoryDlg.DoSelChanged(Wnd: HWND; Item: PItemIDList);
var
  Name: string;
begin
  if FShowSelectionInStatus or assigned(FSelChanged) then
  begin
    Name := '';
    SetLength(Name, MAX_PATH);
    SHGetPathFromIDList(Item, PChar(Name));
    SetLength(Name, StrLen(PChar(Name)));
    if FShowSelectionInStatus then
      StatusText := Name;
    if assigned(FSelChanged) then
      FSelChanged(Self, Name);
  end;
end;

procedure TBrowseDirectoryDlg.SetFitStatusText(Val: boolean);
begin
  if FFitStatusText = Val then exit;
  FFitStatusText := Val;
  // Reset the status text area if needed.
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
end;

procedure TBrowseDirectoryDlg.SetStatusText(const Val: string);
begin
  if FStatusText = Val then exit;
  FStatusText := Val;
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
end;

procedure TBrowseDirectoryDlg.SetSelection(const Val: string);
begin
  if FSelection = Val then exit;
  FSelection := Val;
  // Add trailing backslash so it looks better in the IDE.
  if (FSelection <> '') and (FSelection[Length(FSelection)] <> '\') and
     DirExists(FSelection) then
    FSelection := FSelection + '\';
  if FDlgWnd <> 0 then begin
    if FSelection <> '' then
      SendMessage(FDlgWnd, BFFM_SETSELECTION, 1, LPARAM(FormatSelection(FSelection)));
  end;
end;

procedure TBrowseDirectoryDlg.SetEnableOKButton(Val: boolean);
begin
  if FEnableOKButton = Val then exit;
  FEnableOKButton := Val;
  if FDlgWnd <> 0 then
    // Documentation for BFFM_ENABLEOK is incorrect.  Value sent in LPARAM, not WPARAM.
    SendMessage(FDlgWnd, BFFM_ENABLEOK, 0, LPARAM(FEnableOKButton));
end;

function TBrowseDirectoryDlg.GetCaption: string;
var
  Temp: array[0..255] of char;
begin
  if FDlgWnd <> 0 then
  begin
    SendMessage(FDlgWnd, WM_GETTEXT, SizeOf(Temp), LPARAM(@Temp));
    Result := string(Temp);
  end else
    Result := FCaption;
end;

procedure TBrowseDirectoryDlg.SetCaption(const Val: string);
begin
  FCaption := Val;
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, WM_SETTEXT, 0, LPARAM(FCaption));
end;

procedure TBrowseDirectoryDlg.SetParent(AParent: TWinControl);
begin
  FParent := AParent;
end;

// Note that BOOL <> boolean type.  Important!
function EnumChildWndProc(Child: HWND; Data: LParam): BOOL; stdcall;
const
  STATUS_TEXT_WINDOW_ID = 14147;
type
  PHWND = ^HWND;
begin
  if GetWindowLong(Child, GWL_ID) = STATUS_TEXT_WINDOW_ID then begin
    PHWND(Data)^ := Child;
    Result := FALSE;
  end else
    Result := TRUE;
end;

function TBrowseDirectoryDlg.FittedStatusText: string;
var
  ChildWnd: HWND;
begin
  Result := FStatusText;
  if FFitStatusText then begin
    ChildWnd := 0;
    if FDlgWnd <> 0 then
      // Enumerate all child windows of the dialog to find the status text window.
      EnumChildWindows(FDlgWnd, @EnumChildWndProc, LPARAM(@ChildWnd));
    if (ChildWnd <> 0) and (FStatusText <> '') then
      if DirExists(FStatusText) then
        Result := MinimizeName(ChildWnd, FStatusText)
      else
        Result := MinimizeString(ChildWnd, FStatusText);
  end;
end;



// Component Editor (not really) to allow on the fly testing of the dialog
{
procedure TBrowseDialogEditor.ExecuteVerb(Index: Integer);
begin
  // we only have one verb, so exit if this ain't it
  if Index <> 0 then Exit;
  Edit;
end;

function TBrowseDialogEditor.GetVerb(Index: Integer): AnsiString;
begin
  Result := 'Test Dialog';
end;

function TBrowseDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TBrowseDialogEditor.Edit;
begin
  with TBrowseDirectoryDlg(Component) do
    if Execute then
      MessageDlg(Format('Item selected:'#13#13'%s', [Selection]),
                 mtInformation, [mbOk], 0);
end;

}
procedure Register;
begin
  RegisterComponents('Tetram', [TBrowseDirectoryDlg]);
//  RegisterComponentEditor(TBrowseDirectoryDlg, TBrowseDialogEditor);
end;

end.
