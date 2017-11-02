unit ICUEnumeration;

interface

uses
  System.SysUtils, System.Classes, _enum, _utypes, icu_globals;

type
  TICUEnumerationWrapper<T> = class(TICUObject)
  private
    FEnumeration: PUEnumeration;
    FEof: Boolean;
    function GetCount: Int32;
  protected
    procedure ReleaseEnumeration; virtual;
  public
    constructor Create(UCalendar: PUEnumeration); reintroduce;
    destructor Destroy; override;

    function Next: T; virtual; abstract;

    procedure Reset;

    property Eof: Boolean read FEof;
    property Count: Int32 read GetCount;
  end;

  TICUWideStringEnumeration = class(TICUEnumerationWrapper<WideString>)
  public
    function Next: WideString; override;
  end;

  TICUAnsiStringEnumeration = class(TICUEnumerationWrapper<AnsiString>)
  public
    function Next: AnsiString; override;
  end;

implementation

uses
  _umachine;

{ TICUEnumerationWrapper }

constructor TICUEnumerationWrapper<T>.Create(UCalendar: PUEnumeration);
begin
  FEnumeration := UCalendar;
  Reset;
end;

destructor TICUEnumerationWrapper<T>.Destroy;
begin
  ReleaseEnumeration;
  inherited;
end;

function TICUEnumerationWrapper<T>.GetCount: Int32;
begin
  ResetErrorCode(FStatus);
  Result := uenum_count(FEnumeration, FStatus);
end;

procedure TICUEnumerationWrapper<T>.ReleaseEnumeration;
begin
  if FEnumeration <> nil then
    uenum_close(FEnumeration);
  FEnumeration := nil;
end;

procedure TICUEnumerationWrapper<T>.Reset;
begin
  ResetErrorCode(FStatus);
  uenum_reset(FEnumeration, FStatus);
  FEof := False;
end;

{ TICUWideStringEnumeration }

function TICUWideStringEnumeration.Next: WideString;
var
  buffer: PUChar;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  buffer := uenum_unext(FEnumeration, bufNeeded, FStatus);
  FEof := buffer = nil;
  Result := buffer^;
end;

{ TICUAnsiStringEnumeration }

function TICUAnsiStringEnumeration.Next: AnsiString;
var
  buffer: PAnsiString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  buffer := uenum_next(FEnumeration, bufNeeded, FStatus);
  FEof := buffer = nil;
  Result := buffer^;
end;

end.
