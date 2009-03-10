unit ShwrMgr;

{.$DEFINE CodeGenerator}

interface

uses Classes, AUtils;

type
  TSharewareEditEvent = (seCustName, seLicenseNo, seMasterNo, seRegKey);

  TSharewareEvent = procedure(Sender: TObject; Event: TSharewareEditEvent; var CurrentValue: string) of object;

  TSharewareManager = class(TComponent)
  private
    {$IFDEF CodeGenerator}
    FGenKey: Integer;
    {$ENDIF}
    FAppKey: Integer;
    FCustName: string;
    FLicenseNumber: string;
    FMasterNumber: string;
    FRegistrationCode: string;
    FIsRegistered: PBool;
    FRegKey: string;
    FRegCodeLength, FLicenseLength: Integer;
    FMasterLicense: Boolean;
    {}
    FOnEditing: TSharewareEvent;
    function GetReleaseKey: Integer;
    procedure SetRegCodeLength(Value: Integer);
    procedure SetLicenseLength(Value: Integer);
    {}
    function GetRegistered: Boolean;
    procedure SetRegistered(Value: Boolean);
  protected
    procedure Loaded; override;
    {}
    function IncCharUpper(C: Char; Step: Integer): Char;
    function IncCharAll(C: Char; Step: Integer): Char;
    {}
    function GetCode(Int: Integer): string;
    function LicenseCodeGenerator(License: string): Integer;
    function CustomerCodeGenerator(Customer: string): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    function RegisterApp: Boolean;
    {}
    function IsRegistrationCode(RegistrationCode: string): Boolean;
    {}
    function KeyToString(Key: Integer): string;
    function StringToKey(Str: string): Integer;
    {}
    property ReleaseKey: Integer read GetReleaseKey;
    {}
    {$IFDEF CodeGenerator}
    property GenKey: Integer write FGenKey;
    function GetCurrentRegistrationCode: string;
    {$ENDIF}
  published
    property CustomerName: string read FCustName write FCustName;
    property LicenseNumber: string read FLicenseNumber write FLicenseNumber;
    property MasterNumber: string read FMasterNumber write FMasterNumber;
    property IsRegistered: Boolean read GetRegistered write SetRegistered;
    {}
    property RegistryKey: string read FRegKey write FRegKey;
    {}
    property RegCodeLength: Integer read FRegCodeLength write SetRegCodeLength;
    property LicenseLength: Integer read FLicenseLength write SetLicenseLength;
    {}
    property MasterLicense: Boolean read FMasterLicense write FMasterLicense;
    property OnEditing: TSharewareEvent read FOnEditing write FOnEditing;
    {}
    property AppKey: Integer read FAppKey write FAppKey;
  end;

var
  HelpContextRegister: Integer;

const
  msgCodeInvalide = 'Code invalide.';

implementation

uses Windows, SysUtils, Registry, Controls, Dialogs, Math, FRegApp;

constructor TSharewareManager.Create(AOwner: TComponent);
begin
  FAppKey:= 0;
  FLicenseLength:= 8;
  FRegCodeLength:= 12;
  inherited Create(AOwner);
  { Check up if the app is registered using the informations set-up in Registry }
  New(FIsRegistered);
end;

destructor TSharewareManager.Destroy;
begin
  Dispose(FIsRegistered);
  FIsRegistered:= nil;
  inherited Destroy;
end;

procedure TSharewareManager.Loaded;
begin
  if not (csDesigning in ComponentState) then
    LoadFromRegistry;
end;

function TSharewareManager.GetRegistered: Boolean;
begin
  if FIsRegistered = nil then
    Result:= False
  else begin
    Result:= FIsRegistered^;
    Dispose(FIsRegistered);
    New(FIsRegistered);
    FIsRegistered^:= Result;
  end;
end;

procedure TSharewareManager.SetRegistered(Value: Boolean);
begin
  FIsRegistered^:= Value;
end;

procedure TSharewareManager.LoadFromRegistry;
var R: TRegistry;
begin
  if RegistryKey = '' then Exit;
  R:= TRegistry.Create;
  try
    R.RootKey:= HKEY_LOCAL_MACHINE;
    if R.OpenKey(RegistryKey, False) then
    begin
      CustomerName:= R.ReadString('CustomerName');
      LicenseNumber:= R.ReadString('LicenseNumber');
      MasterNumber:= R.ReadString('MasterNumber');
      FRegistrationCode:= R.ReadString('RegistrationCode');
      if MasterNumber <> '' then
        MasterLicense:= True;
      IsRegistered:= IsRegistrationCode(FRegistrationCode);
    end;
  finally
    R.Free;
  end;
end;

procedure TSharewareManager.SaveToRegistry;
var R: TRegistry;
begin
  if RegistryKey = '' then Exit;
  R:= TRegistry.Create;
  try
    R.RootKey:= HKEY_LOCAL_MACHINE;
    if R.OpenKey(RegistryKey, True) then
    begin
      R.WriteString('CustomerName', CustomerName);
      R.WriteString('LicenseNumber', LicenseNumber);
      R.WriteString('MasterNumber', MasterNumber);
      R.WriteString('RegistrationCode', FRegistrationCode);
    end;
  finally
    R.Free;
  end;
end;

function TSharewareManager.RegisterApp: Boolean;
var FenRegApp: TFenRegApp;
begin
  Result:= False;
  FenRegApp:= TFenRegApp.Create(Self);
  try
    FenRegApp.Manager:= Self;
    FenRegApp.CustomerName:= CustomerName;
    FenRegApp.LicenseNumber:= LicenseNumber;
    FenRegApp.MasterNumber:= MasterNumber;
    FenRegApp.RegCode:= FRegistrationCode;
    FenRegApp.HelpContext:= HelpContextRegister;
    if FenRegApp.ShowModal = mrOk then
    begin
      CustomerName:= FenRegApp.CustomerName;
      LicenseNumber:= FenRegApp.LicenseNumber;
      MasterNumber:= FenRegApp.MasterNumber;
      if MasterNumber <> '' then
        MasterLicense:= True;
      FRegistrationCode:= FenRegApp.RegCode;
      IsRegistered:= IsRegistrationCode(FRegistrationCode);
      if IsRegistered then
        SaveToRegistry
      else
        MessageDlg(msgCodeInvalide, mtError, [mbOk], 0);
      Result:= IsRegistered;
    end;
  finally
    FenRegApp.Free;
  end;
end;

procedure TSharewareManager.SetRegCodeLength(Value: Integer);
begin
  if (Value mod 4) = 0 then
    FRegCodeLength:= Value;
end;

procedure TSharewareManager.SetLicenseLength(Value: Integer);
begin
  if (Value mod 4) = 0 then
    FLicenseLength:= Value;
end;

{ Methods designed to show ReleaseKeys / decode given ReleaseKeys }

function TSharewareManager.IncCharUpper(C: Char; Step: Integer): Char;
var i: Integer;
begin
  Result:= C;
  for i:= 1 to Abs(Step) do
  begin
    if Step > 0 then
    begin
      if Result = '9' then
        Result:= 'A'
      else if Result = 'Z' then
        Result:= '0'
      else
        Inc(Result);
    end else begin
      if Result = '0' then
        Result:= 'Z'
      else if Result = 'A' then
        Result:= '9'
      else
        Dec(Result);
    end;
  end;
end;

function TSharewareManager.IncCharAll(C: Char; Step: Integer): Char;
var i: Integer;
begin
  Result:= C;
  for i:= 1 to Abs(Step) do
  begin
    if Step > 0 then
    begin
      if Result = '9' then
        Result:= 'A'
      else if Result = 'Z' then
        Result:= 'a'
      else if Result = 'z' then
        Result:= '0'
      else
        Inc(Result);
    end else begin
      if Result = '0' then
        Result:= 'z'
      else if Result = 'a' then
        Result:= 'Z'
      else if Result = 'A' then
        Result:= '9'
      else
        Dec(Result);
    end;
  end;
end;

procedure Swap(var Str: string; Idx1, Idx2: Integer);
var C: Char;
begin
  C:= Str[Idx1];
  Str[Idx1]:= Str[Idx2];
  Str[Idx2]:= C;
end;

function TSharewareManager.KeyToString(Key: Integer): string;
var i: Integer;
begin
  { 1 - Code in Hex
    2 - Invert byte by byte the string
    3 - Modify numbers so that they can be anything }
  Result:= IntToHex(Key, 8);
  Result:= ReverseStr(Result);
  for i:= 1 to 8 do
    Result[i]:= IncCharUpper(Result[i], 3 * i + 5);
  Swap(Result, 1, 6);
  Swap(Result, 2, 4);
  Swap(Result, 3, 8);
  Swap(Result, 5, 7);
  Insert('-', Result, 5);
end;

function TSharewareManager.StringToKey(Str: string): Integer;
var i: Integer;
begin
  Delete(Str, 5, 1);
  Swap(Str, 1, 6);
  Swap(Str, 2, 4);
  Swap(Str, 3, 8);
  Swap(Str, 5, 7);
  for i:= 1 to 8 do
    Str[i]:= IncCharUpper(Str[i], - (3 * i + 5));
  Str:= ReverseStr(Str);
  Result:= HexToInt(Str);
end;

{ Key methods to determine whether or not the code is the good one }

function IntToAlpha(I: Integer): Char;
begin
  if I in [0..9] then
    Result:= Char(I + Ord('0'))
  else if I in [10..35] then
    Result:= Char(I + Ord('A') - 10)
  else
    Result:= Char(I + Ord('a') - 36);
end;

procedure MakeThemCrazy(I1, I2: Integer); forward;

function CRC(const S: string): Integer;
const CRCKey = $52EF57A9;
var i: Integer;
    j, CurByte: Byte;
begin
  Result := 0;

  if Length(S) <= 4 then
  begin
    Move(PChar(S)^, Result, Length(S));
    Exit;
  end;

  for i:= 1 to Length(S) do
  begin
    CurByte:= Byte(S[i]);
    Result:= Result xor CurByte;  //ou exclusif des 8 bits de poids faible
    for j:= 1 to 8 do // 8 fois (on entre des octets donc 8 bits)
    begin
      if (Result and 1) = 0 then
        Result:= Result shr 1
      else
        Result:= (Result shr 1) xor CRCKey;
    end;
  end;
end;

function TSharewareManager.CustomerCodeGenerator(Customer: string): Integer;
begin
  Result:= CRC(Customer);
end;

function TSharewareManager.LicenseCodeGenerator(License: string): Integer;
var i: Integer;
    M: Integer;
begin
  M:= 1; Result:= 0;
  for i:= 1 to 8 do
  begin
    Result:= Result + M * (Byte(License[i]) - Ord('0'));
    M:= M * 10;
  end;
end;

function TSharewareManager.GetCode(Int: Integer): string;
var i: Integer;
begin
  Result:= '';
  SetLength(Result, 4);
  Int:= Abs(Int) mod (62 * 62 * 62 * 62);
  for i:= 1 to 4 do
  begin
    Result[i]:= IntToAlpha(Int mod 62);
    Int:= Int div 62;
  end;
end;

function TSharewareManager.IsRegistrationCode(RegistrationCode: string): Boolean;
var UCode: Integer;
    LCode: Integer;
    MCode: Integer;
    RCode: Integer;
begin
  Result:= (Length(LicenseNumber) = LicenseLength) and (Length(CustomerName) > 0) and (Length(RegistrationCode) = RegCodeLength);
  if Result then
  begin
    UCode:= CustomerCodeGenerator(CustomerName);
    LCode:= LicenseCodeGenerator(LicenseNumber);
    RCode:= ReleaseKey;
    UCode:= UCode xor LCode;
    LCode:= LCode xor RCode;
    RCode:= RCode xor UCode;
    MakeThemCrazy(LCode, UCode);
    if MasterLicense then
    begin
      if Length(MasterNumber) = LicenseLength then
      begin
        MCode:= LicenseCodeGenerator(MasterNumber);
        UCode:= UCode xor MCode;
        LCode:= LCode xor MCode;
        RCode:= RCode xor MCode;
      end else
        Exit;
    end;
    UCode:= UCode xor AppKey;
    LCode:= LCode xor AppKey;
    RCode:= RCode xor AppKey;
    Result:= (GetCode(UCode) + GetCode(LCode) + GetCode(RCode)) = RegistrationCode;
  end;
end;

function TSharewareManager.GetReleaseKey: Integer;
var Temp: DWord;
begin
  GetVolumeInformation('C:\', nil, 0, @Result, Temp, Temp, nil, 0);
  {$IFDEF CodeGenerator}
  if FGenKey <> 0 then
    Result:= FGenKey;
  {$ENDIF}
end;

{$IFDEF CodeGenerator}
function TSharewareManager.GetCurrentRegistrationCode: string;
var UCode: Integer;
    LCode: Integer;
    MCode: Integer;
    RCode: Integer;
begin
  Result:= '';
  if (Length(LicenseNumber) = LicenseLength) and (Length(CustomerName) > 0) then
  begin
    UCode:= CustomerCodeGenerator(CustomerName);
    LCode:= LicenseCodeGenerator(LicenseNumber);
    RCode:= ReleaseKey;
    UCode:= UCode xor LCode;
    LCode:= LCode xor RCode;
    RCode:= RCode xor UCode;
    MakeThemCrazy(LCode, UCode);
    if MasterLicense then
    begin
      if Length(MasterNumber) = LicenseLength then
      begin
        MCode:= LicenseCodeGenerator(MasterNumber);
        UCode:= UCode xor MCode;
        LCode:= LCode xor MCode;
        RCode:= RCode xor MCode;
      end else
        Exit;
    end;
    UCode:= UCode xor AppKey;
    LCode:= LCode xor AppKey;
    RCode:= RCode xor AppKey;
    Result:= GetCode(UCode) + GetCode(LCode) + GetCode(RCode);
  end;
end;
{$ENDIF}

procedure MakeThemCrazy(I1, I2: Integer);
begin
{$O-}
  asm
    Call @GetBackAddr
  @GetBackAddr:
    Pop ECX
    Push EBX
    Push ESI
    Push EDI
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
  @L1:
    cmp EAX, EDX
    jb @L3
    ja @L4
  @L2:
    xor EAX, EDX
    call @L5
    jmp @L7
  @L3:
    and EAX, EDX
    call @L6
    jmp @L7
  @L4:
    or EAX, EDX
    jmp @L7
  @L5:
    ret
  @L6:
    ret
  @L7:
    Pop EDI
    Pop ESI
    Pop EBX
  end;
{$O+}
end;

end.
