unit GzFBDBEx;

interface

uses
  Classes, DB, VDataSet, uib;

type
  TUIBDataBaseEx = class(TUIBDataBase)
  private
    FAllowConnection: Boolean;
  protected
    procedure SetConnected(const Value: Boolean); override;
  public
    property AllowConnection: Boolean read FAllowConnection write FAllowConnection;
  end;

implementation

{ TUIBDataBaseEx }

procedure TUIBDataBaseEx.SetConnected(const Value: Boolean);
begin
  if (not (csDesigning in ComponentState)) and (not FAllowConnection) then
    Exit;
  inherited;
end;

end.
