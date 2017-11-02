unit GZRL_FChoixExport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ExitPnl, GZRL_FRequetesLibres;

type
  TExportFormat = (fCSV, fConstText);

  TFenExportDonnees = class(TForm)
    Panel2: TPanel;
    Label4: TLabel;
    btnCSV: TRadioButton;
    btnTextConst: TRadioButton;
    PageControlFormat: TPageControl;
    TabSheetCSV: TTabSheet;
    TabSheetConstText: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    LabelQuote: TLabel;
    CBQuote: TCheckBox;
    EditQuote: TEdit;
    CBLineBreak: TComboBox;
    EditQuoteASCII: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    ExitPanel1: TExitPanel;
    EditSeparator: TEdit;
    EditSeparatorASCII: TEdit;
    SD: TSaveDialog;
    Label3: TLabel;
    procedure btnCSVClick(Sender: TObject);
    procedure btnTextConstClick(Sender: TObject);
    procedure CBQuoteClick(Sender: TObject);
    procedure EditSeparatorChange(Sender: TObject);
    procedure EditQuoteChange(Sender: TObject);
    procedure EditSeparatorASCIIChange(Sender: TObject);
    procedure EditQuoteASCIIChange(Sender: TObject);
    procedure EditSeparatorASCIIExit(Sender: TObject);
    procedure EditQuoteASCIIExit(Sender: TObject);
    procedure CBLineBreakExit(Sender: TObject);
    procedure SDTypeChange(Sender: TObject);
    procedure ExitPanel1OkClick(Sender: TObject);
    procedure PageControlFormatChange(Sender: TObject);
  private
    FFileName: string;
    FFormat: TExportFormat;
    FCSVFormat: TCSVFormat;
    {}
    function ASCIIValue(const S: string): string;
    function ASCIIChar(const S: string): string;
    function ASCIIString(const S: string): string;
    {}
    procedure SetActivePage(Page: TTabSheet);
  public
    property FileName: string read FFileName;
    property Format: TExportFormat read FFormat;
    {}
    property CSVFormat: TCSVFormat read FCSVFormat;
  end;

var
  FenExportDonnees: TFenExportDonnees;

implementation

uses DataSetStreamer;

{$R *.DFM}

function TFenExportDonnees.ASCIIValue(const S: string): string;
begin
  if S = '' then
    Result:= ''
  else
    Result:= '#' + IntToStr(Ord(S[1]));
end;

function TFenExportDonnees.ASCIIChar(const S: string): string;
var i: Integer;
begin
  Result:= '';
  if (Length(S) > 1) and (S[1] = '#') then
  begin
    for i:= 2 to Length(S) do
      if not (S[i] in ['0'..'9']) then
        Exit;
    i:= StrToIntDef(Copy(S, 2, Length(S)), -1);
    if (i >= 0) and (i <= 255) then
      Result:= Char(i);
  end;
end;

function TFenExportDonnees.ASCIIString(const S: string): string;
var i: Integer;
    T: string;
begin
  Result:= '';
  if Length(S) > 1 then
  begin
    if S[1] = '#' then
    begin
      i:= 2;
      while i <= Length(S) do
      begin
        if S[i] = '#' then
          Break;
        if not (S[i] in ['0'..'9']) then
          Exit;
        Inc(i);
      end;
      if i = Length(S) then
        Inc(i);
      T:= Copy(S, 1, i - 1);
      Result:= ASCIIChar(T) + ASCIIString(Copy(S, i, Length(S)));
    end;
  end;
end;

procedure TFenExportDonnees.SetActivePage(Page: TTabSheet);
begin
  PageControlFormat.ActivePage:= Page;
end;

procedure TFenExportDonnees.btnCSVClick(Sender: TObject);
begin
  SetActivePage(TabSheetCSV);
end;

procedure TFenExportDonnees.btnTextConstClick(Sender: TObject);
begin
  SetActivePage(TabSheetConstText);
end;

procedure TFenExportDonnees.CBQuoteClick(Sender: TObject);
begin
  LabelQuote.Enabled:= CBQuote.Checked;
  EditQuote.Enabled:= CBQuote.Checked;
  EditQuoteASCII.Enabled:= CBQuote.Checked;
end;

procedure TFenExportDonnees.EditSeparatorChange(Sender: TObject);
begin
  EditSeparatorASCII.Text:= ASCIIValue(EditSeparator.Text);
end;

procedure TFenExportDonnees.EditQuoteChange(Sender: TObject);
begin
  EditQuoteASCII.Text:= ASCIIValue(EditQuote.Text);
end;

procedure TFenExportDonnees.EditSeparatorASCIIChange(Sender: TObject);
begin
  EditSeparator.Text:= ASCIIChar(EditSeparatorASCII.Text);
end;

procedure TFenExportDonnees.EditQuoteASCIIChange(Sender: TObject);
begin
  EditQuote.Text:= ASCIIChar(EditQuoteASCII.Text);
end;

procedure TFenExportDonnees.EditSeparatorASCIIExit(Sender: TObject);
begin
  if EditSeparator.Text = '' then
    EditSeparatorASCII.Text:= '';
end;

procedure TFenExportDonnees.EditQuoteASCIIExit(Sender: TObject);
begin
  if EditQuote.Text = '' then
    EditQuoteASCII.Text:= '';
end;

procedure TFenExportDonnees.CBLineBreakExit(Sender: TObject);
var S1, S2: string;
    i: Integer;
begin
  S1:= ASCIIString(CBLineBreak.Text);
  S2:= '';
  for i:= 1 to Length(S1) do
    S2:= S2 + ASCIIValue(S1[i]);
  CBLineBreak.Text:= S2;
end;

procedure TFenExportDonnees.SDTypeChange(Sender: TObject);
begin
  case SD.FilterIndex of
    1: SD.DefaultExt:= 'csv';
    2: SD.DefaultExt:= 'txt';
    else
      SD.DefaultExt:= '';
  end;
end;

procedure TFenExportDonnees.ExitPanel1OkClick(Sender: TObject);
begin
  if btnCSV.Checked then
    SD.DefaultExt:= '.csv'
  else
    SD.DefaultExt:= '.txt';
  if SD.Execute then
  begin
    FFileName:= SD.FileName;
    if btnCSV.Checked then
    begin
      FCSVFormat.FieldSeparator:= EditSeparator.Text;
      FCSVFormat.LineBreak:= ASCIIString(CBLineBreak.Text);
      if CBQuote.Checked then
        FCSVFormat.QuoteChar:= EditQuote.Text
      else
        FCSVFormat.QuoteChar:= '';
      FFormat:= fCSV;
    end else
      FFormat:= fConstText;
    ModalResult:= mrOk;
  end;
end;

procedure TFenExportDonnees.PageControlFormatChange(Sender: TObject);
begin
  btnCSV.Checked:= PageControlFormat.ActivePageIndex = 0;
  btnTextConst.Checked:= PageControlFormat.ActivePageIndex = 1;
end;

end.
