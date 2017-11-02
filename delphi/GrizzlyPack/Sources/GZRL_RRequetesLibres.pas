unit GZRL_RRequetesLibres;

interface
                      
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  quickrpt, Qrctrls, DB, DBTables, ExtCtrls, DBGrids, QRCustomGrid;

type
  TRepRequetesLibres = class(TQuickRep)
    DetailBand1: TQRBand;
    PageFooterBand1: TQRBand;
    TitleBand1: TQRBand;
    ColumnHeaderBand1: TQRBand;
    QRTitre: TQRSysData;
    QRSysData2: TQRSysData;
    QRSysData3: TQRSysData;
    QRSysData4: TQRSysData;
    GridTitres: TQRCustomGrid;
    GridDetail: TQRCustomGrid;
    procedure QRSysData3Print(sender: TObject; var Value: String);
    procedure QuickRepEndPage(Sender: TCustomQuickRep);
  private
  public
  end;

var
  RepRequetesLibres: TRepRequetesLibres;

implementation

{$R *.DFM}

procedure TRepRequetesLibres.QRSysData3Print(sender: TObject;
  var Value: String);
begin
  Value:= Value + '/' + IntToStr(DataSet.RecordCount);
end;

procedure TRepRequetesLibres.QuickRepEndPage(Sender: TCustomQuickRep);
begin
  Screen.Cursor := crDefault;
end;

end.
