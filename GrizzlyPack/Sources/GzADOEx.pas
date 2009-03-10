unit GzADOEx;

interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TADOConnectionEx = class(TADOConnection)
  private
    FAllowConnection: Boolean;
  protected
    procedure DoConnect; override;
  public
    property AllowConnection: Boolean read FAllowConnection write FAllowConnection;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Grizzly DB', [TADOConnectionEx]);
end;

{ TADOConnectionEx }

procedure TADOConnectionEx.DoConnect;
begin
  if (not (csDesigning in ComponentState)) and (not FAllowConnection) then
    Exit;
  inherited;
end;

end.
