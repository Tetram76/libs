unit StrCont;

interface

uses
  SysUtils, Classes;

type
  TStringsContainer = class(TComponent)
  private
    { Déclarations privées }
    FStringList : TStringList;
  protected
    { Déclarations protégées }
    function GetText: string;
    procedure SetText(const Value: string);
    function GetStrings : TStrings;
    procedure SetStrings(Value : TStrings);
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Text : string read GetText Write SetText;
  published
    { Déclarations publiées }
    property Strings : TStrings Read GetStrings Write SetStrings;
  end;

implementation

Function TStringsContainer.GetStrings : TStrings;
Begin
  Result := FStringList;
End;

Procedure TStringsContainer.SetStrings(Value : TStrings);
Begin
  If Assigned(FStringList) and Assigned(Value) then
    FStringList.Assign(Value);
End;

Constructor TStringsContainer.Create(AOwner : TComponent);
Begin
  Inherited Create(AOwner);
  FStringList := TStringList.Create;
End;

Destructor TStringsContainer.Destroy;
Begin
  FStringList.Free;
  Inherited Destroy;
End;

function TStringsContainer.GetText: string;
begin
  Result := Strings.Text;
end;

procedure TStringsContainer.SetText(const Value: string);
begin
  Strings.Text := Value;
end;

end.
