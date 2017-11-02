unit MBinToRC;

interface

uses
  SysUtils, Classes, ResStore;

type
  TDMBinToRC = class(TDataModule)
    ExeStore: TRessourceStore;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DMBinToRC: TDMBinToRC;

implementation

{$R *.dfm}

end.
