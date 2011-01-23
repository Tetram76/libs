{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description: adapter unit - converts JvInterpreter calls to delphi calls
             automatically generated by Pas2JvInterpreter

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvInterpreter_JvInterpreter.pas 12461 2009-08-14 17:21:33Z obones $

unit JvInterpreter_JvInterpreter;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/trunk/jvcl/run/JvInterpreter_JvInterpreter.pas $';
    Revision: '$Revision: 12461 $';
    Date: '$Date: 2009-08-14 19:21:33 +0200 (ven., 14 août 2009) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

{ EJvInterpreterError }

{ constructor Create(AErrCode: Integer; AErrPos: Integer; AErrName: string; AErrName2: string) }

procedure EJvInterpreterError_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(EJvInterpreterError.Create(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]));
end;

{ procedure Assign(E: Exception); }

procedure EJvInterpreterError_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EJvInterpreterError(Args.Obj).Assign(V2O(Args.Values[0]) as Exception);
end;

{ procedure Clear; }

procedure EJvInterpreterError_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EJvInterpreterError(Args.Obj).Clear;
end;

{ property Read ErrCode: Integer }

procedure EJvInterpreterError_Read_ErrCode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrCode;
end;

{ property Read ErrPos: Integer }

procedure EJvInterpreterError_Read_ErrPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrPos;
end;

{ property Read ErrName: string }

procedure EJvInterpreterError_Read_ErrName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrName1;
end;

{ property Read ErrName2: string }

procedure EJvInterpreterError_Read_ErrName2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrName2;
end;

{ property Read ErrUnitName: string }

procedure EJvInterpreterError_Read_ErrUnitName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrUnitName;
end;

{ property Read ErrLine: Integer }

procedure EJvInterpreterError_Read_ErrLine(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrLine;
end;

{ property Read Message1: string }

procedure EJvInterpreterError_Read_ErrMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EJvInterpreterError(Args.Obj).ErrMessage;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cJvInterpreter = 'JvInterpreter';
begin
  with JvInterpreterAdapter do
  begin
    { EJvInterpreterError }
    AddClass(cJvInterpreter, EJvInterpreterError, 'EJvInterpreterError');
    AddGet(EJvInterpreterError, 'Create', EJvInterpreterError_Create, 4, [varInteger, varInteger, varString,
      varString], varEmpty);
    AddGet(EJvInterpreterError, 'Assign', EJvInterpreterError_Assign, 1, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'Clear', EJvInterpreterError_Clear, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrCode', EJvInterpreterError_Read_ErrCode, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrPos', EJvInterpreterError_Read_ErrPos, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrName', EJvInterpreterError_Read_ErrName, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrName2', EJvInterpreterError_Read_ErrName2, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrUnitName', EJvInterpreterError_Read_ErrUnitName, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrLine', EJvInterpreterError_Read_ErrLine, 0, [varEmpty], varEmpty);
    AddGet(EJvInterpreterError, 'ErrMessage', EJvInterpreterError_Read_ErrMessage, 0, [varEmpty], varEmpty);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
