unit GzCommandLine;

interface

uses SysUtils;

function GetStringParam(Id: string; Default: string) : string;
function GetIntegerParam(Id: string; Default: Integer) : Integer;
function GetBooleanParam(Id: string; Default: Boolean) : Boolean;

implementation

function GetStringParam(Id: string; Default: string) : string;
var
  i : Integer;
  ANext : string;
begin
  Result := Default;
  if ParamCount = 0 then
    Exit;
  for i := 1 to ParamCount - 1 do
  begin
    if ParamStr(i) = Id then
    begin
      ANext := ParamStr(i + 1);
      if (ANext <> '') then
        Result := ParamStr(i + 1);
      Exit;
    end;
  end;
end;

function GetIntegerParam(Id: string; Default: Integer) : Integer;
var
  v, e : Integer;
begin
  Val(GetStringParam(Id, IntToStr(Default)), v, e);
  if e > 0 then
    Result := Default
  else
    Result := v;
end;

function GetBooleanParam(Id: string; Default: Boolean) : Boolean;
var
  i : Integer;
begin
  Result := Default;
  if ParamCount = 0 then
    Exit;
  for i := 1 to ParamCount do
  begin
    if ParamStr(i) = Id then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
