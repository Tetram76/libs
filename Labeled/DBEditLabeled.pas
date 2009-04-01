unit DBEditLabeled;

{$D-}

interface

uses
  Windows, SysUtils, Classes, DBCtrls, LinkControls;

type
  TDBEditLabeled = class(TDBEdit)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBCheckBoxLabeled = class(TDBCheckBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBMemoLabeled = class(TDBMemo)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBComboBoxLabeled = class(TDBComboBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

implementation

procedure TDBEditLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBEditLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBEditLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDBCheckBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBCheckBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBCheckBoxLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDBMemoLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBMemoLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBMemoLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

{ TDBComboBoxLabeled }

constructor TDBComboBoxLabeled.Create(AOwner: TComponent);
begin
  inherited;
  FLinkControls := TControlList.Create;
end;

destructor TDBComboBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBComboBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBComboBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

procedure TDBComboBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBMemoLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBMemoLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBCheckBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBCheckBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

end.

