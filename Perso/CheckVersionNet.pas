unit CheckVersionNet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  WinInet, StdCtrls, ShellAPI, Dialogs, ExtCtrls, Divers;

type
  TfrmVerifUpgrade = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  protected
    FHomePage, FUpgrade, FProgramme, FVersion: string;
  end;

function CheckVersion(const Titre, Code: string; CurrentVersion: TVersionNumber; ForceMessage, CanContinue: Boolean): Integer;

implementation

{$R *.dfm}

procedure RaiseLastInternetError;
var
  Buffer: array of Char;
  lBuffer: Cardinal;
  ErrorCode: DWord;
begin
  lBuffer := 1024;
  SetLength(Buffer, lBuffer);
  if not InternetGetLastResponseInfo(ErrorCode, @Buffer, lBuffer) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetLength(Buffer, lBuffer);
      if not InternetGetLastResponseInfo(ErrorCode, @Buffer, lBuffer) then RaiseLastOsError;
    end
    else
      RaiseLastOsError;
  end;
  raise EOSError.Create(PChar(@Buffer));
end;

function CheckVersion(const Titre, Code: string; CurrentVersion: TVersionNumber; ForceMessage, CanContinue: Boolean): Integer;
// Valeurs de retour:
// -1: erreur durant l'interrogation du site
// 0: pas de mise à jour
// 1: mise à jour et utilisateur demande à fermer l'appli
const
  FLAG_ICC_FORCE_CONNECTION = 1;
var
  hISession, hRequest: HINTERNET;
  ss: TStringStream;
  BytesRead: Cardinal;
  Buffer: TBytes;
  lBuffer, dDummy: Cardinal;

  sIdent: string;
  sl: TStringList;
  frmVerifUpgrade: TfrmVerifUpgrade;
begin
  Result := 0;
  hISession := InternetOpen(PChar(Format('%s/%s', [Titre, string(CurrentVersion)])), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if (hISession = nil) then RaiseLastOsError;
  try
    hRequest := InternetOpenUrl(hISession, PChar('http://www.tetram.org/lastversion.php?programme=' + Code), nil, 0, INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD or INTERNET_FLAG_RESYNCHRONIZE, 0);
    if (hRequest = nil) then RaiseLastOsError;
    try
      lBuffer := 1024;
      SetLength(Buffer, lBuffer);
      dDummy := 0;
      if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE, Buffer, lBuffer, dDummy) then
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
          SetLength(Buffer, lBuffer);
          if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE, Buffer, lBuffer, dDummy) then RaiseLastOsError;
        end
        else
          RaiseLastOsError;

      ss := TStringStream.Create('', TEncoding.Unicode);
      try
        ss.Size := 0;
        ss.Write(Buffer[0], lBuffer);
        if ss.DataString <> '200' then
        begin
          ss.WriteString(#13#10);
          lBuffer := 1024;
          SetLength(Buffer, lBuffer);
          if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_TEXT, Buffer, lBuffer, dDummy) then
            if GetLastError = ERROR_INSUFFICIENT_BUFFER then
            begin
              SetLength(Buffer, lBuffer);
              if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_TEXT, Buffer, lBuffer, dDummy) then RaiseLastOsError;
            end
            else
              RaiseLastOsError;
          ss.Write(Buffer[0], lBuffer);
          raise EOSError.Create(ss.DataString);
        end;
      finally
        ss.Free;
      end;

      lBuffer := 4096;
      SetLength(Buffer, lBuffer);
      ss := TStringStream.Create('', TEncoding.UTF8);
      try
        while InternetReadFile(hRequest, Buffer, lBuffer, BytesRead) do
        begin
          ss.Write(Buffer[0], BytesRead);
          if BytesRead < lBuffer then Break;
        end;

        sIdent := 'Request: ' + Code;
        ss.Position := 0;
        if ss.ReadString(Length(sIdent)) = sIdent then
        begin
          sl := TStringList.Create;
          try
            sl.NameValueSeparator := ':';
            sl.Text := ss.DataString;
            if CurrentVersion < Trim(sl.Values['Version']) then begin
              frmVerifUpgrade := TfrmVerifUpgrade.Create(nil);
              try
                frmVerifUpgrade.Panel1.Visible := not CanContinue;
                frmVerifUpgrade.Panel2.Visible := CanContinue;
                frmVerifUpgrade.FHomePage := Trim(sl.Values['HomePage']);
                frmVerifUpgrade.FUpgrade := Trim(sl.Values['URL']);
                frmVerifUpgrade.FProgramme := Trim(sl.Values['Name']);
                frmVerifUpgrade.FVersion := Trim(sl.Values['Version']);
                if frmVerifUpgrade.ShowModal = mrOk then Result := 1;
              finally
                frmVerifUpgrade.Free;
              end;
            end
            else if ForceMessage then
              ShowMessage('Votre version est à jour.');
          finally
            sl.Free;
          end;
        end
        else
        begin
          ShowMessage('Impossible d''interroger le site de mise à jour.');
          Result := -1;
        end;
      finally
        ss.Free;
      end;
    finally
      InternetCloseHandle(hRequest);
    end;
  finally
    InternetCloseHandle(hISession);
  end;
end;

procedure TfrmVerifUpgrade.FormShow(Sender: TObject);
begin
  Label1.Visible := FHomePage <> '';
  Label2.Visible := FUpgrade <> '';
  if FProgramme <> '' then
    Label3.Caption := Format('Votre version de %s est obsolète.', [FProgramme, FVersion]);
  if FVersion <> '' then
    Label3.Caption := Label3.Caption + Format(#13#10'Rendez-vous sur le site pour obtenir la dernière version (%s).', [FVersion]);
end;

procedure TfrmVerifUpgrade.Label1Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(FHomePage), nil, nil, SW_NORMAL);
end;

procedure TfrmVerifUpgrade.Label1MouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clRed;
end;

procedure TfrmVerifUpgrade.Label1MouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clBlue;
end;

procedure TfrmVerifUpgrade.Label2Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(FUpgrade), nil, nil, SW_NORMAL);
end;

end.

