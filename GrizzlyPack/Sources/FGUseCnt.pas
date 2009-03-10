unit FGUseCnt;

interface

uses
  Classes, ExtCtrls;

type
  TUseCountActivate = procedure (ActivationValue : Boolean) of object;

  TUseCountWaitMode = (wmForceActivation, wmIgnoreActivation);

  TUseCount = class(TComponent)
  private
    FOnActivate : TUseCountActivate;
		FUseCount : Longint;
    FTimer: TTimer;
    FActDelay, FUnActDelay: LongInt;
    FMode: TUseCountWaitMode;
		procedure SetActive(Value : Boolean);
    function GetActive : Boolean;
    procedure SetActDelay(Value: LongInt);
    procedure SetUnActDelay(Value: LongInt);
    procedure AllocTimer;
    procedure BeforeUse;
  protected
    procedure OnTimer(Sender: TObject); virtual;
  public
    procedure BeginUse;
    procedure EndUse;
  published
    property Active : Boolean read GetActive write SetActive;
    property ActivationDelay: LongInt read FActDelay write SetActDelay;
    property UnActivationDelay: LongInt read FUnActDelay write SetUnActDelay;
    property WaitMode: TUseCountWaitMode read FMode write FMode;
    property OnActivate : TUseCountActivate read FOnActivate write FOnActivate;
  end;

implementation

procedure TUseCount.BeforeUse;
begin
    { Cancels Activation timer if necessary }
  if Assigned(FTimer) then
  begin
    if FTimer.Enabled and (WaitMode = wmForceActivation) then
    begin
      FTimer.Enabled:= False;
      OnTimer(FTimer);
    end else
      FTimer.Enabled:= False;
  end;
end;

procedure TUseCount.BeginUse;
begin
  BeforeUse;
	if FUseCount=0 then
  begin
  	SetActive(True);
  end;
 	Inc(FUseCount);
end;

procedure TUseCount.EndUse;
begin
  BeforeUse;
	if FUseCount<=1 then
  begin
  	SetActive(False);
    FUseCount:=0;
  end
  else
  	Dec(FUseCount);
end;

procedure TUseCount.OnTimer(Sender: TObject);
begin
  FTimer.Enabled:= False;
  if Assigned(FOnActivate) then
    FOnActivate(Active);
end;

procedure TUseCount.SetActive(Value : Boolean);
begin
  if Assigned(FOnActivate) then
  begin
    if Value and (FActDelay > 0) then
    begin
      FTimer.Interval:= FActDelay;
      FTimer.Enabled:= True;
    end else if (not Value) and (FUnActDelay > 0) then
    begin
      FTimer.Interval:= FUnActDelay;
      FTimer.Enabled:= True;
    end else
      FOnActivate(Value);
  end;
end;

function TUseCount.GetActive : Boolean;
begin
  Result:=(FUseCount>0);
end;

procedure TUseCount.AllocTimer;
begin
    { Allocates memory only when a timer is necessary }
  if (FActDelay + FUnActDelay = 0) and Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer:= nil;
  end else if (FActDelay + FUnActDelay <> 0) and (FTimer = nil) then
  begin
    FTimer:= TTimer.Create(Self);
    FTimer.Enabled:= False;
    FTimer.OnTimer:= OnTimer;
  end;
end;

procedure TUseCount.SetActDelay(Value: LongInt);
begin
  if Value < 0 then Exit;
  FActDelay:= Value;
  AllocTimer;
end;

procedure TUseCount.SetUnActDelay(Value: LongInt);
begin
  if Value < 0 then Exit;
  FUnActDelay:= Value;
  AllocTimer;
end;

end.
