unit Unit2;

{$I RX.INC}
{$N+,P+,S-}

interface

uses DB;

{$IFDEF RX_D3}
procedure _DBError(const Msg: string);
{$ELSE}
procedure _DBError(Ident: Word);
{$ENDIF}
procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);

implementation

{$IFDEF RX_D3}
procedure _DBError(const Msg: string);
begin
  DatabaseError(Msg);
{$ELSE}
procedure _DBError(Ident: Word);
begin
  DBError(Ident);
{$ENDIF}
end;

procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
var
  I: Integer;
  F, FSrc: TField;
begin
  if not (Dest.State in dsEditModes) then _DBError(SNotEditing);
  if ByName then begin
    for I := 0 to Source.FieldCount - 1 do begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if F <> nil then begin
{$IFDEF WIN32}
        F.Value := Source.Fields[I].Value;
{$ELSE}
        if (F.DataType = Source.Fields[I].DataType) and
          (F.DataSize = Source.Fields[I].DataSize) then
          F.Assign(Source.Fields[I])
        else F.AsString := Source.Fields[I].AsString;
{$ENDIF}
      end;
    end;
  end
  else begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) then begin
{$IFDEF WIN32}
        F.Value := FSrc.Value;
{$ELSE}
        if F.DataType = FSrc.DataType then F.Assign(FSrc)
        else F.AsString := FSrc.AsString;
{$ENDIF}
      end;
    end;
  end;
end;

end.
