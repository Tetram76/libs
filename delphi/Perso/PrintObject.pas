unit PrintObject;
{.$D-}
interface

uses
  SysUtils, WinSpool, Windows, Classes, Controls, Graphics, Forms, Printers, JPEG, Dialogs, Generics.Collections, System.UITypes;

var
  mf: tmetafile;

const
  ENotPrinting: string = 'The printer is not printing';
  EPrinting: string = 'The printer is printing';
  EObjectPrinting: string = 'The PrintObject is printing';
  EOwnerNotEmpty: string = 'The Owner must exists';
  EOwnerNotWin: string = 'The Owner must be a TWinControl descendant';
  ENoPreview: string = 'OPreview is not defined. Impossible to create preview.';
type
  TBeforeNotifyEvent = procedure(Sender: TObject; var Cancel: Boolean) of object;
  EPrintObject = class(Exception);
  { All the known paper sizes }
  TQRPaperSize = (Default, Letter, LetterSmall, Tabloid, Ledger, Legal, Statement, Executive,
    A3, A4, A4Small, A5, B4, B5, Folio, Quarto, qr10X14, qr11X17, Note, Env9,
    Env10, Env11, Env12, Env14, CSheet, DSheet, ESheet, Custom);

  TQRBin = (First, Upper, Lower, Middle, Manual, Envelope, EnvManual, Auto, Tractor, SmallFormat,
    LargeFormat, LargeCapacity, Cassette, Last);

  TPaperSizesSupported = array[Letter..Custom] of boolean;

  TPrintingState = (psNone, psStarting, psEnding, psAborting, psPrinting);
  TPrintingStates = set of TPrintingState;
  TDrawOption = (doByHeight, doByWidth);

  TTextOption = (toExact);
  TTextOptions = set of TTextOption;

const
  { Actual paper sizes for all the known paper types }
  cQRPaperSizeMetrics: array[Letter..ESheet, 0..1] of extended =
  ((215.9, 279.4), { Letter }
    (215.9, 279.4), { Letter small }
    (279.4, 431.8), { Tabloid }
    (431.8, 279.4), { Ledger }
    (215.9, 355.6), { Legal }
    (139.7, 215.9), { Statement }
    (190.5, 254.0), { Executive }
    (297.0, 420.0), { A3 }
    (210.0, 297.0), { A4 }
    (210.0, 297.0), { A4 small }
    (148.0, 210.0), { A5 }
    (250.0, 354.0), { B4 }
    (182.0, 257.0), { B5 }
    (215.9, 330.2), { Folio }
    (215.0, 275.0), { Quarto }
    (254.0, 355.6), { 10X14 }
    (279.4, 431.8), { 11X17 }
    (215.9, 279.0), { Note }
    (98.43, 225.4), { Envelope #9 }
    (104.8, 241.3), { Envelope #10 }
    (114.3, 263.5), { Envelope #11 }
    (101.6, 279.4), { Envelope #12 - might be wrong !! }
    (127.0, 292.1), { Envelope #14 }
    (100.0, 100.0),
    (100.0, 100.0),
    (100.0, 100.0));

  { Table for translating TQRPaperSize to values which can be used with the printer driver }
  cQRPaperTranslate: array[Default..Custom] of integer =
  (0,
    dmpaper_Letter,
    dmpaper_LetterSmall,
    dmpaper_Tabloid,
    dmpaper_Ledger,
    dmpaper_Legal,
    dmpaper_Statement,
    dmpaper_Executive,
    dmpaper_A3,
    dmpaper_A4,
    dmpaper_A4Small,
    dmpaper_A5,
    dmpaper_B4,
    dmpaper_B5,
    dmpaper_Folio,
    dmpaper_Quarto,
    dmpaper_10X14,
    dmpaper_11X17,
    dmpaper_Note,
    dmpaper_Env_9,
    dmpaper_Env_10,
    dmpaper_Env_11,
    dmpaper_Env_12,
    dmpaper_Env_14,
    dmpaper_CSheet,
    dmpaper_DSheet,
    dmpaper_ESheet,
    $100);

  cQRBinTranslate: array[First..Last] of integer = (1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 14);

type
  TPrintObject = class;
  {-------------------------------------------------------------------------------}
  THeaderRecord = class(TCollectionItem)
  private
    FText: string;
    FPosition: Single;
    FAlignment: TAlignment;
    FFont: TFont;
    procedure SetAlignment(Value: TAlignment);
    procedure SetText(const Value: string);
    procedure SetPosition(Value: Single);
    procedure SetFont(Value: TFont);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Position: Single read FPosition write SetPosition;
  end;
  {-------------------------------------------------------------------------------}
  THeaderRecords = class(TCollection)
  private
    FPrintObject: TPrintObject;
    function GetItem(Index: Integer): THeaderRecord;
    procedure SetItem(Index: Integer; Value: THeaderRecord);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(PrintObject: TPrintObject);
    function Add: THeaderRecord;
    property Items[Index: Integer]: THeaderRecord read GetItem write SetItem; default;
  end;
  {-------------------------------------------------------------------------------}
  TFooterRecord = class(TCollectionItem)
  private
    FText: string;
    FPosition: Single;
    FAlignment: TAlignment;
    FFont: TFont;
    procedure SetAlignment(Value: TAlignment);
    procedure SetText(const Value: string);
    procedure SetPosition(Value: Single);
    procedure SetFont(Value: TFont);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Position: Single read FPosition write SetPosition;
  end;
  {-------------------------------------------------------------------------------}
  TFooterRecords = class(TCollection)
  private
    FPrintObject: TPrintObject;
    function GetItem(Index: Integer): TFooterRecord;
    procedure SetItem(Index: Integer; Value: TFooterRecord);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(PrintObject: TPrintObject);
    function Add: TFooterRecord;
    property Items[Index: Integer]: TFooterRecord read GetItem write SetItem; default;
  end;
  {-------------------------------------------------------------------------------}
  THeaderCoordinates = class(TPersistent)
  private
    Owner: TPrintObject;
    FHeight: Single;
    FWidth: Single;
    FTop: Single;
    FLeft: Single;
    FBottom: Single;
    FRight: Single;
    FBoxed: Boolean;
    FShading: TColor;
    FLineWidth: Word;
    procedure SetHeight(Index: Integer; Value: Single);
    procedure SetWidth(Index: Integer; Value: Single);
  published
    property Top: Single index 0 read FTop write SetHeight;
    property Left: Single index 0 read FLeft write SetWidth;
    property Bottom: Single index 1 read FBottom write SetHeight;
    property Right: Single index 1 read FRight write SetWidth;
    property Height: Single index 2 read FHeight write SetHeight;
    property Width: Single index 2 read FWidth write SetWidth;
    property Boxed: Boolean read FBoxed write FBoxed;
    property BackColor: TColor read FShading write FShading default clWhite;
    property LineWidth: Word read FLineWidth write FLineWidth;
  public
    constructor Create(AOwner: TPrintObject);
    procedure Assign(Source: TPersistent); override;
  end;
  {-------------------------------------------------------------------------------}
  TFooterCoordinates = class(TPersistent)
  private
    Owner: TPrintObject;
    FHeight: Single;
    FWidth: Single;
    FTop: Single;
    FLeft: Single;
    FBottom: Single;
    FRight: Single;
    FBoxed: Boolean;
    FShading: TColor;
    FLineWidth: Word;
    procedure SetHeight(Index: Integer; Value: Single);
    procedure SetWidth(Index: Integer; Value: Single);
  published
    property Top: Single index 0 read FTop write SetHeight;
    property Left: Single index 0 read FLeft write SetWidth;
    property Bottom: Single index 1 read FBottom write SetHeight;
    property Right: Single index 1 read FRight write SetWidth;
    property Height: Single index 2 read FHeight write SetHeight;
    property Width: Single index 2 read FWidth write SetWidth;
    property Boxed: Boolean read FBoxed write FBoxed;
    property BackColor: TColor read FShading write FShading default clWhite;
    property LineWidth: Word read FLineWidth write FLineWidth;
  public
    constructor Create(AOwner: TPrintObject);
    procedure Assign(Source: TPersistent); override;
  end;
  {-------------------------------------------------------------------------------}
  TPageNumberRecord = class(TPersistent)
  private
    Owner: TPrintObject;
    FPrinted: Boolean;
    FText: string;
    FPosition: Single;
    FAlignment: TAlignment;
    FFont: TFont;
    FPrefix: string;
    FSuffix: string;
    procedure SetFont(Value: TFont);
    procedure SetPrefix(Value: string);
    procedure SetSuffix(Value: string);
    procedure MakeFormatNumber;
  public
    constructor Create(AOwner: TPrintObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property Position: Single read FPosition write FPosition;
    property Printed: Boolean read FPrinted write FPrinted;
  end;
  {-------------------------------------------------------------------------------}
  TDateFormat = (dfShortDateFormat, dfLongDateFormat);
  TDateType = (dtStart, dtCurrent);
  TTimeFormat = (tfShortTimeFormat, tfLongTimeFormat);
  TOrder = (DateFirst, TimeFirst);

  TDateTimeRecord = class(TPersistent)
  private
    PrintFormat: string;
    Owner: TPrintObject;
    FPrinted: Boolean;
    FDateFormat: TDateFormat;
    FDatePrinted: Boolean;
    FDateType: TDateType;
    FTimeFormat: TTimeFormat;
    FTimePrinted: Boolean;
    FOrder: TOrder;
    FSeparator: string;
    FPosition: Single;
    FAlignment: TAlignment;
    FFont: TFont;
    procedure SetDate(Value: TDateFormat);
    procedure SetTime(Value: TTimeFormat);
    procedure SetDatePrinted(Value: Boolean);
    procedure SetTimePrinted(Value: Boolean);
    procedure SetOrder(Value: TOrder);
    procedure SetSeparator(Value: string);
    procedure SetFont(Value: TFont);
    procedure MakePrintFormat;
  public
    constructor Create(AOwner: TPrintObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Printed: Boolean read FPrinted write FPrinted;
    property DateFormat: TDateFormat read FDateFormat write SetDate;
    property DatePrinted: Boolean read FDatePrinted write SetDatePrinted;
    property DateType: TDateType read FDateType write FDateType;
    property TimeFormat: TTimeFormat read FTimeFormat write SetTime;
    property TimePrinted: Boolean read FTimePrinted write SetTimePrinted;
    property Order: TOrder read FOrder write SetOrder;
    property Separator: string read FSeparator write SetSeparator;
    property Position: Single read FPosition write FPosition;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
  end;
  {-------------------------------------------------------------------------------}
  TColumnInformationRecord = class(TCollectionItem)
  private
    FLength: Single;
    FPosition: Single;
    FAlignment: TAlignment;
    FFont: TFont;
    procedure SetAlignment(Value: TAlignment);
    procedure SetLength(Value: Single);
    procedure SetPosition(Value: Single);
    procedure SetFont(Value: TFont);
    function GetLength: Single;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Length: Single read GetLength write SetLength;
    property Position: Single read FPosition write SetPosition;
  end;
  {-------------------------------------------------------------------------------}
  TColumnInformationRecords = class(TCollection)
  private
    FPrintObject: TPrintObject;
    function GetItem(Index: Integer): TColumnInformationRecord;
    procedure SetItem(Index: Integer; Value: TColumnInformationRecord);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(PrintObject: TPrintObject);
    function Add: TColumnInformationRecord;
    property Items[Index: Integer]: TColumnInformationRecord read GetItem write SetItem; default;
  end;
  {-------------------------------------------------------------------------------}
  TCurrentFontRecord = class(TCollectionItem)
  private
    FFont: TFont;
    procedure SetFont(Value: TFont);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Font: TFont read FFont write SetFont;
  end;
  {-------------------------------------------------------------------------------}
  TCurrentFontRecords = class(TCollection)
  private
    FPrintObject: TPrintObject;
    function GetItem(Index: Integer): TCurrentFontRecord;
    procedure SetItem(Index: Integer; Value: TCurrentFontRecord);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(PrintObject: TPrintObject);
    function Add(Font: TFont): TCurrentFontRecord;
    property Items[Index: Integer]: TCurrentFontRecord read GetItem write SetItem; default;
  end;
  {-------------------------------------------------------------------------------}
  TMyPoint = record
    x, y: Single;
  end;
  {-------------------------------------------------------------------------------}
  TDetailCoordinates = class(TPersistent)
  private
    Owner: TPrintObject;
    FHeight: Single;
    FWidth: Single;
    FTop: Single;
    FLeft: Single;
    FBottom: Single;
    FRight: Single;
    procedure SetHeight(Index: Integer; Value: Single);
    procedure SetWidth(Index: Integer; Value: Single);
  published
    property Top: Single index 0 read FTop write SetHeight;
    property Left: Single index 0 read FLeft write SetWidth;
    property Bottom: Single index 1 read FBottom write SetHeight;
    property Right: Single index 1 read FRight write SetWidth;
    property Height: Single index 2 read FHeight write SetHeight;
    property Width: Single index 2 read FWidth write SetWidth;
  public
    constructor Create(AOwner: TPrintObject);
    procedure Assign(Source: TPersistent); override;
  end;
  {-------------------------------------------------------------------------------}
  TMarginsPixels = record
    Top, Bottom, Right, Left: Integer;
  end;
  {-------------------------------------------------------------------------------}
  TMarginsMms = class(TPersistent)
  private
    Owner: TPrintObject;
    FTop: Single;
    FLeft: Single;
    FBottom: Single;
    FRight: Single;
    procedure SetHeight(Index: Integer; Value: Single);
    procedure SetWidth(Index: Integer; Value: Single);
  published
    property Top: Single index 0 read FTop write SetHeight;
    property Left: Single index 0 read FLeft write SetWidth;
    property Bottom: Single index 1 read FBottom write SetHeight;
    property Right: Single index 1 read FRight write SetWidth;
  public
    constructor Create(AOwner: TPrintObject);
    procedure Assign(Source: TPersistent); override;
  end;
  {-------------------------------------------------------------------------------}
  TSizeOption = (soAuto, soPrinter, soScreen);
  {-------------------------------------------------------------------------------}
  //  TPageInformations = class(TPersistent)
  //    private
  //      Owner: TPrintObject;
  //      PixelsPerMmVertical: Extended;
  //      PixelsPerMmHorizontal: Extended;
  //      WidthPixels: Extended;
  //      HeightPixels: Extended;
  //      FMarginMm: TMarginsMms;           { Margins in milimeters }
  //      FMargin: TMarginsPixels;          { Margins in pixels }
  //      FBottomMargin: extended;
  //      FLeftMargin: extended;
  //      FHeight: extended;
  //      FOrientation: TPrinterOrientation;
  //      FPaperSize: TQRPaperSize;
  //      FRightMargin: extended;
  //      FTopMargin: extended;
  //      FWidth: extended;
  //      function GetPaperSize: TQRPaperSize;
  //      function GetValue(Index: integer): extended;
  //      procedure SetOrientation(Value: TPrinterOrientation);
  //      procedure SetPaperSize(Value: TQRPaperSize);
  //      procedure SetValue(Index: integer; Value : extended);
  //    protected
  //  //    procedure ReadValues(Reader : TReader); override;
  //  //    procedure SetParentSizes; override;
  //  //    procedure SetUnits(Value : TQRUnit); override;
  //  //    procedure WriteValues(Writer : TWriter); override;
  //    public
  //      constructor Create(AParent : TPrintObject);
  //      destructor Destroy; override;
  //    published
  //      procedure CalculateMeasurements;
  //      property BottomMargin: extended index 0 read GetValue write SetValue;
  //      property LeftMargin: extended index 4 read GetValue write SetValue;
  //      property Height: extended index 1 read GetValue write SetValue;
  //      property Orientation: TPrinterOrientation read FOrientation write SetOrientation;
  //      property PaperSize: TQRPaperSize read GetPaperSize write SetPaperSize;
  //      property RightMargin: extended index 5 read GetValue write SetValue;
  //      property TopMargin: extended index 2 read GetValue write SetValue;
  //      property Width: extended index 3 read GetValue write SetValue;
  //      property MmsMargins: TMarginsMms read FMarginMm write FMarginMm;
  //      property PixelsMargins: TMarginsPixels read FMargin;
  //    end;
  {-------------------------------------------------------------------------------}
  TPrinterSettings = class(TPersistent)
  private
    { Device stuff }
    FDevice: PChar;
    FDriver: PChar;
    FPort: PChar;
    DeviceMode: THandle;
    DevMode: PDeviceMode;
    { Storage variables }
    Owner: TPrintObject;
    FCopies: Integer;
    FOrientation: TPrinterOrientation;
    FDuplex: Boolean;
    FMaxExtentX: Integer;
    FMaxExtentY: Integer;
    FMinExtentX: Integer;
    FMinExtentY: Integer;
    FOutputBin: TQRBin;
    FPaperSize: TQRPaperSize;
    FPaperSizes: TPaperSizesSupported;
    FPaperWidthMms: Single;
    FPaperLengthMms: Single;
    FPixelsPerX: Integer;
    FPixelsPerY: Integer;
    FPixelsPerMmY: Single; { Number of pixels per Mm along Y axis }
    FPixelsPerMmX: Single; { Number of pixels per Mm along X axis }
    FTopOffset: Integer;
    FLeftOffset: Integer;
    FTitle: string;
    FPaperWidthPixels: Integer; { Full width of page in pixels includes gutters }
    FPaperLengthPixels: Integer; { Full height of page in pixels includes gutters }
    Gutter: TMarginsPixels; { Unprintable area }
    function GetCopies: Integer;
    function GetDriver: string;
    function GetDuplex: Boolean;
    function GetMaxExtentX: Integer;
    function GetMaxExtentY: Integer;
    function GetMinExtentX: Integer;
    function GetMinExtentY: Integer;
    function GetOrientation: TPrinterOrientation;
    function GetOutputBin: TQRBin;
    function GetPaperSize: TQRPaperSize;
    function GetPaperSizeSupported(PaperSize: TQRPaperSize): Boolean;
    function GetPaperWidthMms: Single;
    function GetPaperLengthMms: Single;
    function GetPixelsPerX: Integer;
    function GetPixelsPerY: Integer;
    function GetPort: string;
    function GetPrinter: TPrinter;
    function GetTitle: string;
    function GetTopOffset: Integer;
    function GetLeftOffset: Integer;
    function Supported(Setting: DWORD): Boolean;
    procedure SetField(aField: DWORD);
    procedure GetPrinterSettings;
    procedure SetCopies(Value: Integer);
    procedure SetDuplex(Value: Boolean);
    procedure SetOrientation(Value: TPrinterOrientation);
    procedure SetOutputBin(Value: TQRBin);
    procedure SetPaperSize(Value: TQRPaperSize);
    procedure SetPaperLengthMms(Value: Single);
    procedure SetPaperWidthMms(Value: Single);
    procedure SetPrinter(Value: TPrinter);
    procedure SetTitle(Value: string);
  private
    property UsedPrinter: TPrinter read GetPrinter write SetPrinter;
  public
    constructor Create(AOwner: TPrintObject);
    destructor Destroy; override;
    procedure ApplySettings;
    { read only properties }
    property Device: string read GetDriver;
    property Driver: string read GetDriver;
    property LeftOffset: Integer read GetLeftOffset;
    property MaxExtentX: Integer read GetMaxExtentX;
    property MaxExtentY: Integer read GetMaxExtentY;
    property MinExtentX: Integer read GetMinExtentX;
    property MinExtentY: Integer read GetMinExtentY;
    property PaperSizeSupported[PaperSize: TQRPaperSize]: Boolean read GetPaperSizeSupported;
    property PixelsPerX: Integer read GetPixelsPerX;
    property PixelsPerY: Integer read GetPixelsPerY;
    property PixelsPerMmX: Single read FPixelsPerMmx;
    property PixelsPerMmY: Single read FPixelsPerMmY;
    property Port: string read GetPort;
    property TopOffset: Integer read GetTopOffset;
    property WorkSheetLengthMms: Single read FPaperLengthMms;
    property WorkSheetWidthMms: Single read FPaperWidthMms;
    property PaperLengthPixels: Integer read FPaperLengthPixels;
    property PaperWidthPixels: Integer read FPaperWidthPixels;
  published
    { Read/write properties }
    property Copies: Integer read GetCopies write SetCopies;
    property Duplex: Boolean read GetDuplex write SetDuplex;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property OutputBin: TQRBin read GetOutputBin write SetOutputBin;
    property PaperLengthMms: Single read GetPaperLengthMms write SetPaperLengthMms;
    property PaperSize: TQRPaperSize read GetPaperSize write SetPaperSize;
    property PaperWidthMms: Single read GetPaperWidthMms write SetPaperWidthMms;
    property Title: string read GetTitle write SetTitle;
  end;
  {-------------------------------------------------------------------------------}
  IPrintObjectPreview = interface
    ['{B4D76B2C-95CC-4E7C-AA2C-5050A0B15DB7}']
    procedure Start; safecall; // début de l'impression
    procedure Abort; safecall; // Annulation de l'impression
    procedure Quit; safecall; // fin de l'impression
    function Pages: TList<TGraphic>; safecall; // liste des pages (TBitmap)
    procedure SetHeightMM(const Value: Single); safecall;
    procedure SetWidthMM(const Value: Single); safecall;
    procedure SetCaption(const Title: string); safecall;
  end;

  TObjectClass = class of TObject;

  TPrintObject = class(TComponent)
  private
    { Déclarations privées }
    FPrintingState: TPrintingState; { En cours d'impression? }
    FPages: TList<TGraphic>; { liste des pages }
    OPreview: IPrintObjectPreview; { Objet récupérant toutes les actions pour préview }
    FDateTime: TDateTimeRecord; { Informations d'impression de la date }
    FAutoPaging: Boolean; { Passage à la page automatique? }
    FFont: TFont; { Font par défaut }
    FMargin: TMarginsPixels; { Margins in pixels }
    FMarginMm: TMarginsMms; { Margins in milimeters }
    FHeader: THeaderRecords; { Informations d'impression du Tête de page }
    FFooter: TFooterRecords; { Informations d'impression du Pied de page }
    FColumnInformation: TColumnInformationRecords; { Informations d'impression des colonnes }
    FDetail: TDetailCoordinates; { Coordonnées du Detail }
    FHeaderCoordinates: THeaderCoordinates; { Coordonnées du Tête de page }
    FFooterCoordinates: TFooterCoordinates; { Coordonnées du Pied de page }
    FPageNumber: TPageNumberRecord; { Informations d'impression du numéro de page }
    FSizeOption: TSizeOption; { Option de calcul de la taille de la feuille d'apperçu }
    //      FPageInfos: TPageInformations;    { Taille de la page }
    FPrinterSettings: TPrinterSettings;

    FBeforeStart: TBeforeNotifyEvent;
    FAfterStart: TNotifyEvent;
    FBeforeQuit: TBeforeNotifyEvent;
    FAfterQuit: TNotifyEvent;
    FBeforeAbort: TBeforeNotifyEvent;
    FAfterAbort: TNotifyEvent;
    FBeforeNewPage: TBeforeNotifyEvent;
    FAfterNewPage: TNotifyEvent;
    FBeforeHeader: TBeforeNotifyEvent;
    FAfterHeader: TNotifyEvent;
    FBeforeFooter: TBeforeNotifyEvent;
    FAfterFooter: TNotifyEvent;
    FBeforeDateTime: TBeforeNotifyEvent;
    FAfterDateTime: TNotifyEvent;
    FBeforePageNumber: TBeforeNotifyEvent;
    FAfterPageNumber: TNotifyEvent;
    FOnDestroy: TNotifyEvent;

    StartDateTimePrint: TDateTime; { Début d'impression }
    CurrentTab: Single; { The value of the current tab }
    CurrentFont: TCurrentFontRecords; { Historique des Font en cours d'utilisation }
    PrevFont: TFont; { Dernière Font utilisée pour imprimer }
    FTitre: string; { Titre de la fenêtre d'apperçu ou du gestionnaire d'impression }
    LastYPosition: Single; { The Y position where the lastwrite occurred }
    TextMetrics: TTextMetric; { Tailles du texte en fonction de la Font courante }

    FPreviewCanvas: TMetafileCanvas;

    {== Configuration ==}
    procedure CalculateMeasurements;
    procedure CalculateTextMetrics;
    procedure SetAutoPaging(Value: Boolean);
    procedure SetColumnInformations(Value: TColumnInformationRecords);
    procedure SetDetailCoordinates(Value: TDetailCoordinates);
    procedure SetFooters(Value: TFooterRecords);
    procedure SetFont(Value: TFont);
    procedure SetHeaders(Value: THeaderRecords);
    procedure SetPreview(Value: IPrintObjectPreview);
    procedure SetSizeOption(Value: TSizeOption);
    function CreatePage: TGraphic;

    {== Dessin ==}
    procedure _DrawBox(XTop, YTop, XBottom, YBottom: Integer; LineWidth: Word; Shading: TColor);
    procedure _PrintPic(dstX, dstY, dstWidth, dstHeight: Integer; Graphic: TGraphic);

    {== Impression ==}
    procedure _WriteText(Canvas: TCanvas; Text: string; X, Y: Integer);
    function CalculateLineHeight: Integer;
    function GetLineHeightPixels: Word;

    function GetPrintingEx(States: TPrintingStates = []): Boolean;
    function GetPrinting: Boolean;
    procedure CheckPrinting(States: TPrintingStates = []);
    procedure SetTitre(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoDestroy; virtual;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    {== Configuration ==}
    procedure CreateColumn1(Number: Word; Position, Length: Single; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
    procedure CreateColumn2(Number: Integer; Position, Length: Single; Alignment: TAlignment; Font: TFont);
    function GetFontName: string;
    function GetFontSize: Word;
    procedure GetGutter(var Top, Bottom, Left, Right: Word);
    function LoadPrinter(FileName: TFileName): Boolean;
    function Position(X, Y: Single): TPoint;
    function SavePrinter(FileName: TFileName): Boolean;
    procedure SetDateTimeInformation1(Position: Single; FormatDate: TDateFormat; DatePrinted: Boolean; DateType: TDateType; FormatTime: TTimeFormat; TimePrinted: Boolean; Order: TOrder; Separator: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
    procedure SetDateTimeInformation2(Position: Single; FormatDate: TDateFormat; DatePrinted: Boolean; DateType: TDateType; FormatTime: TTimeFormat; TimePrinted: Boolean; Order: TOrder; Separator: string; Alignment: TAlignment; Font: TFont);
    procedure SetDetailTopBottom(Top, Bottom: Single);
    procedure SetFooterDimensions1(Left, Right, Bottom, Height: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
    procedure SetFooterDimensions2(Left, Right, Bottom, Top: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
    procedure SetFooterInformation1(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
    procedure SetFooterInformation2(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; Font: TFont);
    procedure SetHeaderDimensions1(Left, Right, Top, Height: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
    procedure SetHeaderDimensions2(Left, Right, Top, Bottom: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
    procedure SetHeaderInformation1(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
    procedure SetHeaderInformation2(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; Font: TFont);
    procedure SetMargins(Top, Bottom, Left, Right: Single);
    procedure SetOrientation(Orient: TPrinterOrientation);
    procedure SetPageNumberInformation1(Position: Single; Prefix, Suffix: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
    procedure SetPageNumberInformation2(Position: Single; Prefix, Suffix: string; Alignment: TAlignment; Font: TFont);
    procedure SetTab(Milimeters: Single);

    {== Ecriture ==}
    procedure WriteColumn(ColumnNumber: Word; Y: Single; Text: string; NbLignes: Integer = 0; TextOptions: TTextOptions = []);
    procedure WriteLine(X, Y: Single; Text: string);
    procedure WriteLineCenter(Y: Single; Text: string);
    procedure WriteLineRight(Y: Single; Text: string);
    procedure WriteLineColumn(ColumnNumber: Word; Y: Single; Text: string);
    procedure WriteLineColumnLeft(ColumnNumber: Word; Y: Single; Text: string);
    procedure WriteLineColumnCenter(ColumnNumber: Word; Y: Single; Text: string);
    procedure WriteLineColumnRight(ColumnNumber: Word; Y: Single; Text: string);
    procedure WriteRotatedLine(X, Y: Single; Text: string; Font: TFont; Angle: Integer);

    {== Dessin ==}
    procedure Draw(Left, Top: Single; Graphic: TGraphic); overload;
    procedure Draw(Left, Top, Value: Single; Graphic: TGraphic; DrawOption: TDrawOption = doByHeight); overload;
    procedure Draw(Left, Top, Height, Width: Single; Graphic: TGraphic); overload;
    procedure DrawBox(XTop, YTop, XBottom, YBottom: Single; LineWidth: Word);
    procedure DrawBoxShaded(XTop, YTop, XBottom, YBottom: Single; LineWidth: Word; Shading: TColor);
    procedure DrawLine(TopX, TopY, BottomX, BottomY: Single; LineWidth: Word; Color: TColor);
    function GetLineWidth: Word;
    procedure SetLineWidth(Width: Word);
    procedure StretchDraw(Left, Top, Width, Height: Single; Graphic: TGraphic);

    {== Impression ==}
    procedure Abort;
    function Destination: TCanvas;
    function GetColumnsPerLine: Integer;
    function GetColumnsPerLineFont(Font: TFont): Integer;
    function GetLineHeightMms: Single;
    function GetLineHeightMmsFont(Font: TFont): Single; overload;
    function GetLineHeightMmsFont(FontName: string; FontSize: Word; FontStyle: TFontStyles): Single; overload;
    function GetLinesInDetailArea: Word;
    function GetLinesInDetailAreaFont(Font: TFont): Word;
    function GetLinesLeft: Word;
    function GetLinesLeftFont(Font: TFont): Word;
    function GetHeightLeftMms: Single;
    function GetHeightLeftPixel: Integer;
    function GetLinesPerPage: Integer;
    function GetLinesPerPageFont(Font: TFont): Integer;
    function GetLineText1(var Start: Integer; Line: Word; Len: Single; Text: string; FontName: string; FontSize: Word; FontStyle: TFontStyles; var FinTexte: Boolean; TextOptions: TTextOptions = []): string;
    function GetLineText2(var Start: Integer; Line: Word; Len: Single; Text: string; Font: TFont; var FinTexte: Boolean; TextOptions: TTextOptions = []): string;
    function GetPageNumber: Integer;
    function GetTextWidthFont(Text: string; Font: TFont): Single;
    function GetTextWidth(Text: string): Single;
    function GetTitre: string;
    function GetYPosition: Single;
    function MmsToPixelsHorizontal(Milimeters: Single): Integer;
    function MmsToPixelsVertical(Milimeters: Single): Integer;
    procedure NewLines(Number: Single);
    procedure NewLinesFont(Number: Word; Font: TFont);
    procedure NewPage;
    procedure NextLine;
    procedure NextLineFont(Font: TFont);
    procedure PreviousLine;
    procedure PreviousLineFont(Font: TFont);
    function PixelsToMmsHorizontal(Pixels: Integer): Single;
    function PixelsToMmsVertical(Pixels: Integer): Single;
    procedure Quit;
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetFontInformation1(FontName: string; FontSize: Word; FontStyle: TFontStyles);
    procedure SetFontInformation2(Font: TFont);
    procedure SetTopOfPage;
    procedure SetYPosition(YPosition: Single);
    procedure Start(Title: string);
    procedure WriteDateTime;
    procedure WriteFooter;
    procedure WriteHeader;
    procedure WritePageNumber;

    procedure PrintPages(Pages: TList<TGraphic>);
  published
    { Déclarations publiées }
    property AutoPaging: Boolean read FAutoPaging write SetAutoPaging default True;
    property Columns: TColumnInformationRecords read FColumnInformation write SetColumnInformations;
    property DateTime: TDateTimeRecord read FDateTime write FDateTime;
    property Detail: TDetailCoordinates read FDetail write SetDetailCoordinates;
    property FooterCoordinates: TFooterCoordinates read FFooterCoordinates write FFooterCoordinates;
    property Footers: TFooterRecords read FFooter write SetFooters;
    property Font: TFont read FFont write SetFont;
    property HeaderCoordinates: THeaderCoordinates read FHeaderCoordinates write FHeaderCoordinates;
    property Headers: THeaderRecords read FHeader write SetHeaders;
    property Margin: TMarginsMms read FMarginMm write FMarginMm;
    //      property Page: TPageInformations read FPageInfos write FPageInfos;
    property PageNumber: TPageNumberRecord read FPageNumber write FPageNumber;
    property PreviewObject: IPrintObjectPreview read OPreview write SetPreview default nil;
    property PrinterSettings: TPrinterSettings read FPrinterSettings write FPrinterSettings;
    property Printing: Boolean read GetPrinting default False;
    property PrintingState: TPrintingState read FPrintingState default psNone;
    property SizeOption: TSizeOption read FSizeOption write SetSizeOption default soAuto;
    property Titre: string read GetTitre write SetTitre;

    property BeforeStart: TBeforeNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeQuit: TBeforeNotifyEvent read FBeforeQuit write FBeforeQuit;
    property AfterQuit: TNotifyEvent read FAfterQuit write FAfterQuit;
    property BeforeAbort: TBeforeNotifyEvent read FBeforeAbort write FBeforeAbort;
    property AfterAbort: TNotifyEvent read FAfterAbort write FAfterAbort;
    property BeforeNewPage: TBeforeNotifyEvent read FBeforeNewPage write FBeforeNewPage;
    property AfterNewPage: TNotifyEvent read FAfterNewPage write FAfterNewPage;
    property BeforeHeader: TBeforeNotifyEvent read FBeforeHeader write FBeforeHeader;
    property AfterHeader: TNotifyEvent read FAfterHeader write FAfterHeader;
    property BeforeFooter: TBeforeNotifyEvent read FBeforeFooter write FBeforeFooter;
    property AfterFooter: TNotifyEvent read FAfterFooter write FAfterFooter;
    property BeforeDateTime: TBeforeNotifyEvent read FBeforeDateTime write FBeforeDateTime;
    property AfterDateTime: TNotifyEvent read FAfterDateTime write FAfterDateTime;
    property BeforePageNumber: TBeforeNotifyEvent read FBeforePageNumber write FBeforePageNumber;
    property AfterPageNumber: TNotifyEvent read FAfterPageNumber write FAfterPageNumber;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

procedure Register;
procedure RaiseError(const Msg: string);

implementation

uses Contnrs;

function CleanString(s: string): string;
begin
  while (Length(s) > 0) and ((s[Length(s)] = #13) or (s[Length(s)] = #10)) do
    SetLength(s, Length(s) - 1);
  Result := s;
end;

function Max(a, b: Variant): Variant;
begin
  if a > b then
    result := a
  else
    result := b;
end;

{############### TMarginsMms ###################################################}

procedure TMarginsMms.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then
    SourceName := 'nil'
  else
    SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TMarginsMms) then
    raise EConvertError.CreateFmt('%s <> %s', [SourceName, ClassName]);
  Left := TMarginsMms(Source).Left;
  Top := TMarginsMms(Source).Top;
  Bottom := TMarginsMms(Source).Bottom;
  Right := TMarginsMms(Source).Right;
end;
{-------------------------------------------------------------------------------}

constructor TMarginsMms.Create(AOwner: TPrintObject);
begin
  Owner := AOwner;
  Top := 0;
  Left := 0;
  Right := 0;
  Bottom := 0;
end;
{-------------------------------------------------------------------------------}

procedure TMarginsMms.SetHeight(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top);
        if (Value < temp) then Value := temp;
        if (FTop <> Value) and (Value + FBottom < Owner.PrinterSettings.WorkSheetLengthMms) then FTop := Value;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        if (Value < temp) then Value := temp;
        if (FBottom <> Value) and (Value + FTop < Owner.PrinterSettings.WorkSheetLengthMms) then FBottom := Value;
      end;
  end;
  Owner.FMargin.Top := Owner.MmsToPixelsVertical(FTop);
  Owner.FMargin.Bottom := Owner.MmsToPixelsVertical(FBottom);
end;
{-------------------------------------------------------------------------------}

procedure TMarginsMms.SetWidth(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Left);
        if (Value < temp) then Value := temp;
        if (FLeft <> Value) and (Value + FRight < Owner.PrinterSettings.WorkSheetWidthMms) then FLeft := Value;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        if (Value < temp) then Value := temp;
        if (FRight <> Value) and (Value + FLeft < Owner.PrinterSettings.WorkSheetWidthMms) then FRight := Value;
      end;
  end;
  if Owner.FDetail <> nil then
  begin
    if Owner.FDetail.Left < FLeft then Owner.FDetail.Left := FLeft;
    if Owner.FDetail.Right < FRight then Owner.FDetail.Right := FRight;
  end;
  Owner.FMargin.Left := Owner.MmsToPixelsHorizontal(FLeft);
  Owner.FMargin.Right := Owner.MmsToPixelsHorizontal(FRight);
end;
{############### TMarginsMms ###################################################}

{############### THeaderCoordinates ############################################}

procedure THeaderCoordinates.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then
    SourceName := 'nil'
  else
    SourceName := Source.ClassName;
  if (Source = nil) or not (Source is THeaderCoordinates) then
    raise EConvertError.CreateFmt('%s <> %s', [SourceName, ClassName]);
  Left := THeaderCoordinates(Source).Left;
  Top := THeaderCoordinates(Source).Top;
  Bottom := THeaderCoordinates(Source).Bottom;
  Right := THeaderCoordinates(Source).Right;
  Boxed := THeaderCoordinates(Source).Boxed;
  BackColor := THeaderCoordinates(Source).BackColor;
  LineWidth := THeaderCoordinates(Source).LineWidth;
end;
{-------------------------------------------------------------------------------}

constructor THeaderCoordinates.Create(AOwner: TPrintObject);
begin
  Owner := AOwner;
  Top := 0;
  Left := 0;
  Right := 0;
  Bottom := 0;
  Boxed := False;
  BackColor := 0;
  LineWidth := 1;
end;
{-------------------------------------------------------------------------------}

procedure THeaderCoordinates.SetHeight(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top);
        if (Value < temp) then Value := temp;
        if (FTop <> Value) and (Value + FBottom < Owner.PrinterSettings.WorkSheetLengthMms) then FTop := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        if (Value < temp) then Value := temp;
        if (FBottom <> Value) and (Value + FTop < Owner.PrinterSettings.WorkSheetLengthMms) then FBottom := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    2:
      begin
        if (FTop + Value > Owner.PrinterSettings.WorkSheetLengthMms) then Value := Owner.PrinterSettings.WorkSheetLengthMms - FTop - Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        FHeight := Value;
        Bottom := Owner.PrinterSettings.WorkSheetLengthMms - FTop - Value;
      end;
  end;
end;
{-------------------------------------------------------------------------------}

procedure THeaderCoordinates.SetWidth(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Left);
        if (Value < temp) then Value := temp;
        if (FLeft <> Value) and (Value + FRight < Owner.PrinterSettings.WorkSheetWidthMms) then FLeft := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        if (Value < temp) then Value := temp;
        if (FRight <> Value) and (Value + FLeft < Owner.PrinterSettings.WorkSheetWidthMms) then FRight := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    2:
      begin
        if (FLeft + Value > Owner.PrinterSettings.WorkSheetWidthMms) then Value := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        FWidth := Value;
        Right := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Value;
      end;
  end;
end;
{############### THeaderCoordinates ############################################}

{############### TFooterCoordinates ############################################}

procedure TFooterCoordinates.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then
    SourceName := 'nil'
  else
    SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TFooterCoordinates) then
    raise EConvertError.CreateFmt('%s <> %s', [SourceName, ClassName]);
  Left := TFooterCoordinates(Source).Left;
  Top := TFooterCoordinates(Source).Top;
  Bottom := TFooterCoordinates(Source).Bottom;
  Right := TFooterCoordinates(Source).Right;
  Boxed := TFooterCoordinates(Source).Boxed;
  BackColor := TFooterCoordinates(Source).BackColor;
  LineWidth := TFooterCoordinates(Source).LineWidth;
end;
{-------------------------------------------------------------------------------}

constructor TFooterCoordinates.Create(AOwner: TPrintObject);
begin
  Owner := AOwner;
  Top := 0;
  Left := 0;
  Right := 0;
  Bottom := 0;
  Boxed := False;
  BackColor := 0;
  LineWidth := 1;
end;
{-------------------------------------------------------------------------------}

procedure TFooterCoordinates.SetHeight(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top);
        if (Value < temp) then Value := temp;
        if (FTop <> Value) and (Value + FBottom < Owner.PrinterSettings.WorkSheetLengthMms) then FTop := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        if (Value < temp) then Value := temp;
        if (FBottom <> Value) and (Value + FTop < Owner.PrinterSettings.WorkSheetLengthMms) then FBottom := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    2:
      begin
        if (Owner.PrinterSettings.WorkSheetLengthMms - FBottom - Value < Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top)) then Value := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top);
        FHeight := Value;
        Top := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - Value;
      end;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TFooterCoordinates.SetWidth(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Left);
        if (Value < temp) then Value := temp;
        if (FLeft <> Value) and (Value + FRight < Owner.PrinterSettings.WorkSheetWidthMms) then FLeft := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        if (Value < temp) then Value := temp;
        if (FRight <> Value) and (Value + FLeft < Owner.PrinterSettings.WorkSheetWidthMms) then FRight := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    2:
      begin
        if (FLeft + Value > Owner.PrinterSettings.WorkSheetWidthMms) then Value := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        FWidth := Value;
        Right := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Value;
      end;
  end;
end;
{############### TFooterCoordinates ############################################}

{############### TDetailCoordinates ############################################}

procedure TDetailCoordinates.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then
    SourceName := 'nil'
  else
    SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TDetailCoordinates) then
    raise EConvertError.CreateFmt('%s <> %s', [SourceName, ClassName]);
  Left := TDetailCoordinates(Source).Left;
  Top := TDetailCoordinates(Source).Top;
  Bottom := TDetailCoordinates(Source).Bottom;
  Right := TDetailCoordinates(Source).Right;
end;
{-------------------------------------------------------------------------------}

constructor TDetailCoordinates.Create(AOwner: TPrintObject);
begin
  Owner := AOwner;
  Top := 0;
  Left := 0;
  Right := 0;
  Bottom := 0;
end;
{-------------------------------------------------------------------------------}

procedure TDetailCoordinates.SetHeight(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Top);
        if (Value < temp) then Value := temp;
        if (FTop <> Value) and (Value + FBottom < Owner.PrinterSettings.WorkSheetLengthMms) then FTop := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        if (Value < temp) then Value := temp;
        if (FBottom <> Value) and (Value + FTop < Owner.PrinterSettings.WorkSheetLengthMms) then FBottom := Value;
        FHeight := Owner.PrinterSettings.WorkSheetLengthMms - FBottom - FTop;
      end;
    2:
      begin
        if (FTop + Value > Owner.PrinterSettings.WorkSheetLengthMms) then Value := Owner.PrinterSettings.WorkSheetLengthMms - FTop - Owner.PixelsToMmsVertical(Owner.PrinterSettings.Gutter.Bottom);
        FHeight := Value;
        Bottom := Owner.PrinterSettings.WorkSheetLengthMms - FTop - Value;
      end;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TDetailCoordinates.SetWidth(Index: Integer; Value: Single);
var
  temp: Single;
begin
  case Index of
    0:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Left);
        if (Value < temp) then Value := temp;
        if (FLeft <> Value) and (Value + FRight < Owner.PrinterSettings.WorkSheetWidthMms) then FLeft := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    1:
      begin
        temp := Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        if (Value < temp) then Value := temp;
        if (FRight <> Value) and (Value + FLeft < Owner.PrinterSettings.WorkSheetWidthMms) then FRight := Value;
        FWidth := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - FRight;
      end;
    2:
      begin
        if (FLeft + Value > Owner.PrinterSettings.WorkSheetWidthMms) then Value := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Owner.PixelsToMmsHorizontal(Owner.PrinterSettings.Gutter.Right);
        FWidth := Value;
        Right := Owner.PrinterSettings.WorkSheetWidthMms - FLeft - Value;
      end;
  end;
end;
{############### TDetailCoordinates ############################################}

{############### TPageNumberRecord #############################################}

procedure TPageNumberRecord.Assign;
begin
  if Source is TPageNumberRecord then
  begin
    Printed := TPageNumberRecord(Source).Printed;
    Prefix := TPageNumberRecord(Source).Prefix;
    Suffix := TPageNumberRecord(Source).Suffix;
    Position := TPageNumberRecord(Source).Position;
    Alignment := TPageNumberRecord(Source).Alignment;
    Font := TPageNumberRecord(Source).Font;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor TPageNumberRecord.Create;
begin
  Owner := AOwner;
  FFont := TFont.Create;
  Prefix := 'Page ';
end;
{-------------------------------------------------------------------------------}

destructor TPageNumberRecord.Destroy;
begin
  FFont.Free;
end;
{-------------------------------------------------------------------------------}

procedure TPageNumberRecord.MakeFormatNumber;
begin
  FText := FPrefix + '%d' + FSuffix;
end;
{-------------------------------------------------------------------------------}

procedure TPageNumberRecord.SetFont;
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPageNumberRecord.SetPrefix;
begin
  FPrefix := Value;
  MakeFormatNumber;
end;
{-------------------------------------------------------------------------------}

procedure TPageNumberRecord.SetSuffix;
begin
  FSuffix := Value;
  MakeFormatNumber;
end;
{############### TPageNumberRecord #############################################}

{############### TDateTimeRecord ###############################################}

procedure TDateTimeRecord.Assign;
begin
  if Source is TDateTimeRecord then
  begin
    Printed := TDateTimeRecord(Source).Printed;
    DateFormat := TDateTimeRecord(Source).DateFormat;
    DatePrinted := TDateTimeRecord(Source).DatePrinted;
    TimeFormat := TDateTimeRecord(Source).TimeFormat;
    TimePrinted := TDateTimeRecord(Source).TimePrinted;
    Order := TDateTimeRecord(Source).Order;
    Separator := TDateTimeRecord(Source).Separator;
    Position := TDateTimeRecord(Source).Position;
    Alignment := TDateTimeRecord(Source).Alignment;
    Font := TDateTimeRecord(Source).Font;
    MakePrintFormat;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor TDateTimeRecord.Create;
begin
  Owner := AOwner;
  FFont := TFont.Create;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

destructor TDateTimeRecord.Destroy;
begin
  FFont.Free;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.MakePrintFormat;

  function IIfDateTime(Test: boolean; TrueValue, FalseValue: Variant): Variant;
  begin
    if test then
      Result := TrueValue
    else
      Result := FalseValue;
  end;
var
  Tf, Df: string;
  sp: string;
begin
  if FDateFormat = dfShortDateFormat then
    Df := FormatSettings.ShortDateFormat
  else
    Df := FormatSettings.LongDateFormat;
  if FTimeFormat = tfShortTimeFormat then
    Tf := FormatSettings.ShortTimeFormat
  else
    Tf := FormatSettings.LongTimeFormat;
  sp := FSeparator;
  while Pos('''', sp) > 0 do
    sp := Copy(sp, 1, Pos('''', sp)) + '''' + Copy(sp, Pos('''', sp) + 1, Length(sp) - Pos('''', sp));
  case FOrder of
    DateFirst: PrintFormat := IIfDateTime(FDatePrinted, Df, '') + IIfDateTime(Length(sp) > 0, sp, ' ') + IIfDateTime(FTimePrinted, Tf, '');
    TimeFirst: PrintFormat := IIfDateTime(FTimePrinted, Tf, '') + IIfDateTime(Length(sp) > 0, sp, ' ') + IIfDateTime(FDatePrinted, Df, '');
  end;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetDate(Value: TDateFormat);
begin
  FDateFormat := Value;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetDatePrinted(Value: Boolean);
begin
  FDatePrinted := Value;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetOrder(Value: TOrder);
begin
  FOrder := Value;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetSeparator(Value: string);
begin
  FSeparator := Value;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetTime(Value: TTimeFormat);
begin
  FTimeFormat := Value;
  MakePrintFormat;
end;
{-------------------------------------------------------------------------------}

procedure TDateTimeRecord.SetTimePrinted(Value: Boolean);
begin
  FTimePrinted := Value;
  MakePrintFormat;
end;
{############### TDateTimeRecord ###############################################}

{############### THeaderRecord #################################################}

procedure THeaderRecord.Assign;
begin
  if Source is THeaderRecord then
  begin
    Text := THeaderRecord(Source).Text;
    Position := THeaderRecord(Source).Position;
    Alignment := THeaderRecord(Source).Alignment;
    Font := THeaderRecord(Source).Font;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor THeaderRecord.Create;
begin
  inherited Create(Collection);
  FFont := TFont.Create;
end;
{-------------------------------------------------------------------------------}

destructor THeaderRecord.Destroy;
begin
  FFont.Free;
  inherited;
end;
{-------------------------------------------------------------------------------}

function THeaderRecord.GetDisplayName;
begin
  Result := Text;
  if Result = '' then Result := inherited GetDisplayName;
end;
{-------------------------------------------------------------------------------}

procedure THeaderRecord.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure THeaderRecord.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure THeaderRecord.SetPosition(Value: Single);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure THeaderRecord.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;
{############### THeaderRecord #################################################}

{############### THeaderRecords ################################################}

function THeaderRecords.Add: THeaderRecord;
begin
  Result := THeaderRecord(inherited Add);
end;
{-------------------------------------------------------------------------------}

constructor THeaderRecords.Create(PrintObject: TPrintObject);
begin
  inherited Create(THeaderRecord);
  FPrintObject := PrintObject;
end;
{-------------------------------------------------------------------------------}

function THeaderRecords.GetItem(Index: Integer): THeaderRecord;
begin
  Result := THeaderRecord(inherited GetItem(Index));
end;
{-------------------------------------------------------------------------------}

function THeaderRecords.GetOwner: TPersistent;
begin
  Result := FPrintObject;
end;
{-------------------------------------------------------------------------------}

procedure THeaderRecords.SetItem(Index: Integer; Value: THeaderRecord);
begin
  inherited SetItem(Index, Value);
end;
{############### THeaderRecords ################################################}

{############### TFooterRecord #################################################}

procedure TFooterRecord.Assign;
begin
  if Source is TFooterRecord then
  begin
    Text := TFooterRecord(Source).Text;
    Position := TFooterRecord(Source).Position;
    Alignment := TFooterRecord(Source).Alignment;
    Font := TFooterRecord(Source).Font;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor TFooterRecord.Create;
begin
  inherited Create(Collection);
  FFont := TFont.Create;
end;
{-------------------------------------------------------------------------------}

destructor TFooterRecord.Destroy;
begin
  FFont.Free;
  inherited;
end;
{-------------------------------------------------------------------------------}

function TFooterRecord.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then Result := inherited GetDisplayName;
end;
{-------------------------------------------------------------------------------}

procedure TFooterRecord.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TFooterRecord.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TFooterRecord.SetPosition(Value: Single);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TFooterRecord.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;
{############### TFooterRecord #################################################}

{############### TFooterRecords ################################################}

function TFooterRecords.Add: TFooterRecord;
begin
  Result := TFooterRecord(inherited Add);
end;
{-------------------------------------------------------------------------------}

constructor TFooterRecords.Create(PrintObject: TPrintObject);
begin
  inherited Create(TFooterRecord);
  FPrintObject := PrintObject;
end;
{-------------------------------------------------------------------------------}

function TFooterRecords.GetItem(Index: Integer): TFooterRecord;
begin
  Result := TFooterRecord(inherited GetItem(Index));
end;
{-------------------------------------------------------------------------------}

function TFooterRecords.GetOwner: TPersistent;
begin
  Result := FPrintObject;
end;
{-------------------------------------------------------------------------------}

procedure TFooterRecords.SetItem(Index: Integer; Value: TFooterRecord);
begin
  inherited SetItem(Index, Value);
end;
{############### TFooterRecords ################################################}

{############### TColumnInformationRecord ######################################}

procedure TColumnInformationRecord.Assign(Source: TPersistent);
begin
  if Source is TColumnInformationRecord then
  begin
    Length := TColumnInformationRecord(Source).Length;
    Position := TColumnInformationRecord(Source).Position;
    Alignment := TColumnInformationRecord(Source).Alignment;
    Font := TColumnInformationRecord(Source).Font;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor TColumnInformationRecord.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFont := TFont.Create;
end;
{-------------------------------------------------------------------------------}

destructor TColumnInformationRecord.Destroy;
begin
  FFont.Free;
  inherited;
end;
{-------------------------------------------------------------------------------}

function TColumnInformationRecord.GetDisplayName: string;
begin
  Result := '#' + IntToStr(Index) + ' TColumnInformationRecord';
end;
{-------------------------------------------------------------------------------}

function TColumnInformationRecord.GetLength: Single;
begin
  Result := FLength;
  if FLength = -1 then
    Result := TColumnInformationRecords(Collection).FPrintObject.Detail.Width - Position;
end;
{-------------------------------------------------------------------------------}

procedure TColumnInformationRecord.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TColumnInformationRecord.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TColumnInformationRecord.SetLength(Value: Single);
begin
  if FLength <> Value then
  begin
    FLength := Value;
    Changed(False);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TColumnInformationRecord.SetPosition(Value: Single);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;
{############### TColumnInformationRecord ######################################}

{############### TColumnInformationRecords #####################################}

function TColumnInformationRecords.Add;
begin
  Result := TColumnInformationRecord(inherited Add);
end;
{-------------------------------------------------------------------------------}

constructor TColumnInformationRecords.Create;
begin
  inherited Create(TColumnInformationRecord);
  FPrintObject := PrintObject;
end;
{-------------------------------------------------------------------------------}

function TColumnInformationRecords.GetItem;
begin
  Result := TColumnInformationRecord(inherited GetItem(Index));
end;
{-------------------------------------------------------------------------------}

function TColumnInformationRecords.GetOwner;
begin
  Result := FPrintObject;
end;
{-------------------------------------------------------------------------------}

procedure TColumnInformationRecords.SetItem;
begin
  inherited SetItem(Index, Value);
end;
{############### TColumnInformationRecords #####################################}

{############### TCurrentFontRecord ############################################}

procedure TCurrentFontRecord.Assign;
begin
  if Source is TCurrentFontRecord then
  begin
    Font := TCurrentFontRecord(Source).Font;
    Exit;
  end;
  inherited Assign(Source);
end;
{-------------------------------------------------------------------------------}

constructor TCurrentFontRecord.Create;
begin
  inherited Create(Collection);
  FFont := TFont.Create;
end;
{-------------------------------------------------------------------------------}

destructor TCurrentFontRecord.Destroy;
begin
  FFont.Destroy;
  inherited;
end;
{-------------------------------------------------------------------------------}

procedure TCurrentFontRecord.SetFont;
begin
  FFont.Assign(Value);
end;
{############### TCurrentFontRecord ############################################}

{############### TCurrentFontRecords ###########################################}

function TCurrentFontRecords.Add;
begin
  Result := TCurrentFontRecord(inherited Add);
  Result.SetFont(Font);
end;
{-------------------------------------------------------------------------------}

constructor TCurrentFontRecords.Create;
begin
  inherited Create(TCurrentFontRecord);
  FPrintObject := PrintObject;
end;
{-------------------------------------------------------------------------------}

function TCurrentFontRecords.GetItem;
begin
  Result := TCurrentFontRecord(inherited GetItem(Index));
end;
{-------------------------------------------------------------------------------}

function TCurrentFontRecords.GetOwner;
begin
  Result := FPrintObject;
end;
{-------------------------------------------------------------------------------}

procedure TCurrentFontRecords.SetItem;
begin
  inherited SetItem(Index, Value);
end;
{############### TCurrentFontRecords ###########################################}

{############### TPrintObject ##################################################}
{=============== Configuration =================================================}

procedure TPrintObject.BeforeDestruction;
begin
  DoDestroy;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.CalculateMeasurements;
begin
  PrinterSettings.GetPrinterSettings;

  Detail.Width := PrinterSettings.WorkSheetWidthMms;
  Detail.Height := PrinterSettings.WorkSheetLengthMms;

  CalculateTextMetrics;

  SetMargins(Margin.Top, Margin.Bottom, Margin.Left, Margin.Right);

  if FHeaderCoordinates.FTop < PixelsToMmsVertical(PrinterSettings.Gutter.Top) then FHeaderCoordinates.Top := PixelsToMmsVertical(PrinterSettings.Gutter.Top);
  if FHeaderCoordinates.FBottom < PixelsToMmsVertical(PrinterSettings.Gutter.Bottom) then FHeaderCoordinates.Bottom := PixelsToMmsVertical(PrinterSettings.Gutter.Bottom);
  if FHeaderCoordinates.FLeft < PixelsToMmsHorizontal(PrinterSettings.Gutter.Left) then FHeaderCoordinates.Left := PixelsToMmsHorizontal(PrinterSettings.Gutter.Left);
  if FHeaderCoordinates.FRight < PixelsToMmsHorizontal(PrinterSettings.Gutter.Right) then FHeaderCoordinates.Right := PixelsToMmsHorizontal(PrinterSettings.Gutter.Right);

  if FFooterCoordinates.FTop < PixelsToMmsVertical(PrinterSettings.Gutter.Top) then FFooterCoordinates.Top := PixelsToMmsVertical(PrinterSettings.Gutter.Top);
  if FFooterCoordinates.FBottom < PixelsToMmsVertical(PrinterSettings.Gutter.Bottom) then FFooterCoordinates.Bottom := PixelsToMmsVertical(PrinterSettings.Gutter.Bottom);
  if FFooterCoordinates.FLeft < PixelsToMmsHorizontal(PrinterSettings.Gutter.Left) then FFooterCoordinates.Left := PixelsToMmsHorizontal(PrinterSettings.Gutter.Left);
  if FFooterCoordinates.FRight < PixelsToMmsHorizontal(PrinterSettings.Gutter.Right) then FFooterCoordinates.Right := PixelsToMmsHorizontal(PrinterSettings.Gutter.Right);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.CalculateTextMetrics;
begin
  if (csDesigning in ComponentState) or not Printing then
    GetTextMetrics(PrinterSettings.UsedPrinter.Handle, TextMetrics)
  else
    GetTextMetrics(Destination.Handle, TextMetrics);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.CheckPrinting(States: TPrintingStates = []);
begin
  if not GetPrinting then RaiseError(ENotPrinting);
end;
{-------------------------------------------------------------------------------}

constructor TPrintObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrinterSettings := TPrinterSettings.Create(Self);
  FMarginMm := TMarginsMms.Create(Self);
  FDetail := TDetailCoordinates.Create(Self);
  FHeaderCoordinates := THeaderCoordinates.Create(Self);
  FFooterCoordinates := TFooterCoordinates.Create(Self);
  CalculateMeasurements;
  FAutoPaging := True;
  FPrintingState := psNone;
  OPreview := nil;
  FSizeOption := soAuto;
  FFont := TFont.Create;
  PrevFont := TFont.Create;
  PrevFont.Assign(FFont);
  FHeader := THeaderRecords.Create(Self);
  FFooter := TFooterRecords.Create(Self);
  FColumnInformation := TColumnInformationRecords.Create(Self);
  //  FPageInfos := TPageInformations.Create(Self);
  FPageNumber := TPageNumberRecord.Create(Self);
  FDateTime := TDateTimeRecord.Create(Self);
  CurrentFont := TCurrentFontRecords.Create(Self);
  FPreviewCanvas := nil;

  SetMargins(0, 0, 0, 0);
  FHeaderCoordinates.BackColor := clWhite;
  FFooterCoordinates.BackColor := clWhite;

  FPages := TObjectList<TGraphic>.Create(True);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.CreateColumn1(Number: Word; Position, Length: Single; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Ft.Color := Color;
    CreateColumn2(Number, Position, Length, Alignment, Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.CreateColumn2(Number: Integer; Position, Length: Single; Alignment: TAlignment; Font: TFont);
var
  s: TColumnInformationRecord;
begin
  s := FColumnInformation.Add;
  s.SetPosition(Position);
  s.SetLength(Length);
  s.SetAlignment(Alignment);
  s.SetFont(Font);
  if Number >= 0 then s.SetIndex(Number);
end;
{-------------------------------------------------------------------------------}

destructor TPrintObject.Destroy;
begin
  CurrentFont.Free;
  FDateTime.Free;
  FPageNumber.Free;
  FFooterCoordinates.Free;
  FHeaderCoordinates.Free;
  FDetail.Free;
  FMarginMm.Free;
  //  FPageInfos.Free;
  FColumnInformation.Free;
  FFooter.Free;
  FHeader.Free;
  PrevFont.Free;
  FFont.Free;
  FPrinterSettings.Free;

  FPages.Free;

  inherited Destroy;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.DoDestroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    Application.HandleException(Self);
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetFontName;
begin
  Result := Destination.Font.Name;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetFontSize;
begin
  Result := Destination.Font.Size;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.GetGutter(var Top, Bottom, Left, Right: Word);
begin
  Top := PrinterSettings.Gutter.Top;
  Bottom := PrinterSettings.Gutter.Bottom;
  Left := PrinterSettings.Gutter.Left;
  Right := PrinterSettings.Gutter.Right;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.Position(X, Y: Single): TPoint;
begin
  Result.x := MmsToPixelsHorizontal(X);
  Result.y := MmsToPixelsVertical(Y);
  if not Assigned(OPreview) then
  begin
    Dec(Result.x, PrinterSettings.Gutter.Left);
    Dec(Result.y, PrinterSettings.Gutter.Top);
  end;
  //  if OptionSize = 2 then begin
  //    Result.x := MulDiv(Result.x, ResolutionEcran, ResolutionPrinterX);
  //    Result.y := MulDiv(Result.y, ResolutionEcran, ResolutionPrinterY);
  //  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.PreviousLine;
begin
  LastYPosition := LastYPosition - GetLineHeightMms;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.PreviousLineFont(Font: TFont);
begin
  LastYPosition := LastYPosition - GetLineHeightMmsFont(Font);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.LoadPrinter(FileName: TFileName): Boolean;
type
  arrayofchar = array[0..255] of char;

  function RetrouveNomEntier(NomCourt: string): arrayofchar;
  var
    I, J: Integer;
    P: string;
  begin
    FillChar(Result, SizeOf(Result), #0);
    for i := 0 to PrinterSettings.UsedPrinter.Printers.Count - 1 do
    begin
      p := PrinterSettings.UsedPrinter.Printers[i];
      if Copy(p, 1, Length(NomCourt)) = NomCourt then
      begin
        for j := 1 to Length(p) do
          Result[j - 1] := p[j];
        Exit;
      end;
    end;
  end;

var
  MyHandle: THandle;
  MyDevMode: PDeviceMode;
  MyDevice,
    MyDriver,
    MyPort: arrayofChar;
  f: file of TDeviceMode;
  LastIoResult: Integer;
begin
  Result := False;
  PrinterSettings.UsedPrinter.PrinterIndex := -1;
  PrinterSettings.UsedPrinter.GetPrinter(MyDevice, MyDriver, MyPort, MyHandle);
  if FileExists(FileName) then
  begin
    MyDevMode := GlobalLock(MyHandle);
    try
      AssignFile(f, FileName);
{$I-}
      Reset(f);
{$I+}
      LastIoResult := IoResult;
      if LastIoResult = 0 then read(f, MyDevMode^);
      CloseFile(f);
      MyDevice := RetrouveNomEntier(MyDevMode^.dmDeviceName); // MyDevMode^.dmDeviceName  ne contient pas le nom complet !!!
      PrinterSettings.UsedPrinter.SetPrinter(MyDevice, MyDriver, MyPort, MyHandle);
      Result := True;
      CalculateMeasurements;
    finally
      GlobalUnlock(MyHandle);
    end;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.SavePrinter(FileName: TFileName): Boolean;
var
  MyHandle: THandle;
  MyDevMode: PDeviceMode;
  MyDevice,
    MyDriver,
    MyPort: array[0..255] of char;
  f: file of TDeviceMode;
  LastIoResult: Integer;
begin
  PrinterSettings.UsedPrinter.GetPrinter(MyDevice, MyDriver, MyPort, MyHandle);
  MyDevMode := GlobalLock(MyHandle);
  try
    AssignFile(f, FileName);
{$I-}
    Rewrite(f);
{$I+}
    LastIoResult := IoResult;
    if LastIoResult = 0 then write(f, MyDevMode^);
    CloseFile(f);
    Result := True;
  finally
    GlobalUnlock(MyHandle);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetAutoPaging(Value: Boolean);
begin
  FAutoPaging := Value;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetColumnInformations(Value: TColumnInformationRecords);
begin
  FColumnInformation.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetDateTimeInformation1(Position: Single; FormatDate: TDateFormat; DatePrinted: Boolean; DateType: TDateType; FormatTime: TTimeFormat; TimePrinted: Boolean; Order: TOrder; Separator: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Ft.Color := Color;
    SetDateTimeInformation2(Position, FormatDate, DatePrinted, DateType, FormatTime, TimePrinted, Order, Separator, Alignment, Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetDateTimeInformation2(Position: Single; FormatDate: TDateFormat; DatePrinted: Boolean; DateType: TDateType; FormatTime: TTimeFormat; TimePrinted: Boolean; Order: TOrder; Separator: string; Alignment: TAlignment; Font: TFont);
begin
  DateTime.FDateFormat := FormatDate;
  DateTime.FDatePrinted := DatePrinted;
  DateTime.FDateType := DateType;
  DateTime.FTimeFormat := FormatTime;
  DateTime.FTimePrinted := TimePrinted;
  DateTime.FSeparator := Separator;
  DateTime.Order := Order;
  DateTime.Position := Position;
  DateTime.Alignment := Alignment;
  DateTime.SetFont(Font);
  DateTime.Printed := True;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetDetailCoordinates(Value: TDetailCoordinates);
begin
  if FDetail <> Value then FDetail.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetDetailTopBottom(Top, Bottom: Single);
begin
  if Top = -1 then Top := Margin.Top + HeaderCoordinates.Height;
  if Bottom = -1 then Bottom := Margin.Bottom + FooterCoordinates.Height;
  if Top + Bottom > PrinterSettings.WorkSheetLengthMms then Exit;
  Detail.Top := Top;
  Detail.Bottom := Bottom;
  LastYPosition := Top - GetLineHeightMms;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFooterDimensions1(Left, Right, Bottom, Height: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
begin
  if Left = -1 then Left := Margin.Left;
  FFooterCoordinates.Left := Left;
  if Right = -1 then Right := Margin.Right;
  FFooterCoordinates.Right := Right;
  if Bottom = -1 then Bottom := Margin.Bottom;
  FFooterCoordinates.Bottom := Bottom;
  FFooterCoordinates.Height := Height;
  FFooterCoordinates.Boxed := Boxed;
  FFooterCoordinates.LineWidth := LineWidth;
  FFooterCoordinates.BackColor := BackColor;
  SetDetailTopBottom(-1, -1);
  SetTopOfPage;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFooterDimensions2(Left, Right, Bottom, Top: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
begin
  if Left = -1 then Left := Margin.Left;
  FFooterCoordinates.Left := Left;
  if Right = -1 then Right := Margin.Right;
  FFooterCoordinates.Right := Right;
  if Bottom = -1 then Bottom := Margin.Bottom;
  FFooterCoordinates.Bottom := Bottom;
  if Top = -1 then Top := Margin.Top;
  FFooterCoordinates.Top := Top;
  FFooterCoordinates.Boxed := Boxed;
  FFooterCoordinates.LineWidth := LineWidth;
  FFooterCoordinates.BackColor := BackColor;
  SetDetailTopBottom(-1, -1);
  SetTopOfPage;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFooterInformation1(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Ft.Color := Color;
    SetFooterInformation2(Line, Position, Text, Alignment, Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFooterInformation2(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; Font: TFont);
var
  s: TFooterRecord;
begin
  s := FFooter.Add;
  s.SetText(Text);
  s.SetPosition(Position);
  s.SetAlignment(Alignment);
  s.SetFont(Font);
  if Line >= 0 then s.SetIndex(Line);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFooters(Value: TFooterRecords);
begin
  FFooter.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetHeaderDimensions1(Left, Right, Top, Height: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
begin
  if Left = -1 then Left := Margin.Left;
  FHeaderCoordinates.Left := Left;
  if Right = -1 then Right := Margin.Right;
  FHeaderCoordinates.Right := Right;
  if Top = -1 then Top := Margin.Top;
  FHeaderCoordinates.Top := Top;
  FHeaderCoordinates.Height := Height;
  FHeaderCoordinates.Boxed := Boxed;
  FHeaderCoordinates.LineWidth := LineWidth;
  FHeaderCoordinates.BackColor := BackColor;
  SetDetailTopBottom(-1, -1);
  SetTopOfPage;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetHeaderDimensions2(Left, Right, Top, Bottom: Single; Boxed: Boolean; LineWidth: Word; BackColor: TColor);
begin
  if Left = -1 then Left := Margin.Left;
  FHeaderCoordinates.Left := Left;
  if Right = -1 then Right := Margin.Right;
  FHeaderCoordinates.Right := Right;
  if Top = -1 then Top := Margin.Top;
  FHeaderCoordinates.Top := Top;
  if Bottom = -1 then Bottom := Margin.Bottom;
  FHeaderCoordinates.Bottom := Bottom;
  FHeaderCoordinates.Boxed := Boxed;
  FHeaderCoordinates.LineWidth := LineWidth;
  FHeaderCoordinates.BackColor := BackColor;
  SetDetailTopBottom(-1, -1);
  SetTopOfPage;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetHeaderInformation1(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Ft.Color := Color;
    SetHeaderInformation2(Line, Position, Text, Alignment, Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetHeaderInformation2(Line: Integer; Position: Single; Text: string; Alignment: TAlignment; Font: TFont);
var
  s: THeaderRecord;
begin
  s := FHeader.Add;
  s.SetText(Text);
  s.SetPosition(Position);
  s.SetAlignment(Alignment);
  s.SetFont(Font);
  if Line >= 0 then s.SetIndex(Line);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetHeaders(Value: THeaderRecords);
begin
  FHeader.Assign(Value);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetMargins(Top, Bottom, Left, Right: Single); { Défini les marges haute, basse, gauche et droite en MMS ! }
begin
  if (Left + Right >= PrinterSettings.WorkSheetWidthMms) then
  begin
    Left := PixelsToMmsHorizontal(PrinterSettings.Gutter.Left);
    Right := PixelsToMmsHorizontal(PrinterSettings.Gutter.Right);
  end;
  if (Left <= 0) then Left := PixelsToMmsHorizontal(PrinterSettings.Gutter.Left);
  if (Right <= 0) then Right := PixelsToMmsHorizontal(PrinterSettings.Gutter.Right);

  if (Top + Bottom >= PrinterSettings.WorkSheetLengthMms) then
  begin
    Top := PixelsToMmsVertical(PrinterSettings.Gutter.Top);
    Bottom := PixelsToMmsVertical(PrinterSettings.Gutter.Bottom);
  end;
  if (Top <= 0) then Top := PixelsToMmsVertical(PrinterSettings.Gutter.Top);
  if (Bottom <= 0) then Bottom := PixelsToMmsVertical(PrinterSettings.Gutter.Bottom);

  Margin.Top := Top;
  Margin.Bottom := Bottom;
  Margin.Left := Left;
  Margin.Right := Right;

  Detail.Left := Left;
  Detail.Right := Right;
  if FDetail.FTop < Top then FDetail.Top := Top;
  if FDetail.FBottom < Bottom then FDetail.Bottom := Bottom;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetOrientation(Orient: TPrinterOrientation);
begin
  //  if PrinterSettings.Orientation = Orient then Exit;
  if Printing and (PrinterSettings.Orientation <> Orient) then NewPage;
  PrinterSettings.Orientation := Orient;
  CalculateMeasurements;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetPageNumberInformation1(Position: Single; Prefix, Suffix: string; Alignment: TAlignment; FontName: string; FontSize: Word; FontStyle: TFontStyles; Color: TColor = clBlack);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Ft.Color := Color;
    SetPageNumberInformation2(Position, Prefix, Suffix, Alignment, Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetPageNumberInformation2(Position: Single; Prefix, Suffix: string; Alignment: TAlignment; Font: TFont);
begin
  PageNumber.Prefix := Prefix;
  PageNumber.Suffix := Suffix;
  PageNumber.Position := Position;
  PageNumber.Alignment := Alignment;
  PageNumber.SetFont(Font);
  PageNumber.Printed := True;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetPreview(Value: IPrintObjectPreview);
begin
  if Printing then RaiseError(EObjectPrinting);
  OPreview := Value;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetSizeOption(Value: TSizeOption);
begin
  if Printing then RaiseError(EObjectPrinting);
  FSizeOption := Value;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetTab(Milimeters: Single);
begin
  CurrentTab := Milimeters;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.CreatePage: TGraphic;
begin
  if Assigned(FPreviewCanvas) then FreeAndNil(FPreviewCanvas);

  Result := TMetaFile.Create;

  TMetafile(Result).Width := PrinterSettings.PaperWidthPixels;
  TMetafile(Result).Height := PrinterSettings.PaperLengthPixels;

  FPreviewCanvas := TMetafileCanvas.Create(TMetafile(Result), PrinterSettings.UsedPrinter.Handle);

  FPreviewCanvas.Font.PixelsPerInch := PrinterSettings.FPixelsPerX;

  if Assigned(OPreview) then
    OPreview.Pages.Add(Result)
  else
    FPages.Add(Result);
end;
{============== Configuration ==================================================}

{============== Ecriture =======================================================}

procedure TPrintObject._WriteText(Canvas: TCanvas; Text: string; X, Y: Integer);
var
  //  LastSize: Integer;
  l, t: Integer;
  LastBkMode: Integer;
begin
  l := X;
  t := Y;
  if Assigned(OPreview) then
  begin
    Inc(l, PrinterSettings.Gutter.Left);
    Inc(t, PrinterSettings.Gutter.Top);
  end;
  LastBkMode := GetBkMode(Canvas.Handle);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  //  case OptionSize of
  //    1:
  Canvas.TextOut(l, t, CleanString(Text));
  //    else begin
  //        LastSize := Canvas.Font.Height; // plus précis que Size
  //        Canvas.Font.Height := MulDiv(LastSize, ResolutionEcran, ResolutionPrinterY);
  //        Canvas.TextOut(MulDiv(l, ResolutionEcran, ResolutionPrinterX),
  //          MulDiv(t, ResolutionEcran, ResolutionPrinterY),
  //          CleanString(Text));
  //        Canvas.Font.Height := LastSize;
  //      end;
  //  end;
  SetBkMode(Canvas.Handle, LastBkMode);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteColumn(ColumnNumber: Word; Y: Single; Text: string; NbLignes: Integer = 0; TextOptions: TTextOptions = []);
var
  FinImpress: Boolean;
  i, NumLigne: Integer;
  p, LastY: Single;
begin
  i := 1;
  NumLigne := 0;
  LastY := GetYPosition;
  p := Y;
  repeat
    Inc(NumLigne);
    WriteLineColumn(ColumnNumber, p, GetLineText2(i, 1, Columns[ColumnNumber].Length, Text, Columns[ColumnNumber].Font, FinImpress, TextOptions));
    p := -1;
  until FinImpress or ((NbLignes > 0) and (NumLigne >= NbLignes));
  if Y = -2 then SetYPosition(LastY);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLine(X, Y: Single; Text: string); { Ecrit Text à la position X, Y (en MM). }
var
  XPixels: Integer;
  YPixels: Integer;
begin
  if (X >= 0.0) then
    XPixels := MmsToPixelsHorizontal(X)
  else
    XPixels := FMargin.Left;
  XPixels := Max(XPixels, PrinterSettings.Gutter.Left);
  if (CurrentTab > 0.0) then Inc(XPixels, MmsToPixelsHorizontal(CurrentTab));

  if (Y < 0) and (Y <> -2.0) and (Y <> -1.0) then Exit;
  if (Y = -1.0) then
  begin
    LastYPosition := LastYPosition + Max(GetLineHeightMmsFont(PrevFont), GetLineHeightMms);
    if (FAutoPaging = True) then
      if (LastYPosition > PrinterSettings.WorkSheetLengthMms - Detail.Bottom) then NewPage;
    YPixels := MmsToPixelsVertical(LastYPosition);
  end
  else
  begin
    if (Y = -2.0) then
    begin
      if (FAutoPaging = True) then
        if (LastYPosition > PrinterSettings.WorkSheetLengthMms - Detail.Bottom) then NewPage;
      YPixels := MmsToPixelsVertical(LastYPosition);
    end
    else
    begin
      YPixels := MmsToPixelsVertical(Y);
      if (YPixels < PrinterSettings.Gutter.Top) then YPixels := PrinterSettings.Gutter.Top;
      if (YPixels > MmsToPixelsVertical(PrinterSettings.WorkSheetLengthMms) - PrinterSettings.Gutter.Bottom) then YPixels := MmsToPixelsVertical(PrinterSettings.WorkSheetLengthMms) - PrinterSettings.Gutter.Bottom;
      LastYPosition := PixelsToMmsVertical(YPixels);
    end;
  end;
  _WriteText(Destination, Text, XPixels - PrinterSettings.Gutter.Left, YPixels - PrinterSettings.Gutter.Top);
  PrevFont.Assign(Destination.Font);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineCenter(Y: Single; Text: string);
var
  PixelLength: Integer;
  StartPixel: Integer;
begin
  PixelLength := Destination.TextWidth(Text);
  StartPixel := ((Max(MmsToPixelsHorizontal(Margin.Left), PrinterSettings.Gutter.Left) + (MmsToPixelsHorizontal(PrinterSettings.WorkSheetWidthMms) - Max(MmsToPixelsHorizontal(Margin.Right), PrinterSettings.Gutter.Right))) div 2) - (PixelLength div 2);

  SetTab(0.0);
  WriteLine(PixelsToMmsHorizontal(StartPixel), Y, Text);
  SetTab(CurrentTab);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineRight(Y: Single; Text: string);
var
  PixelLength: Word;
  StartPixel: Word;
begin
  PixelLength := Destination.TextWidth(Text);
  StartPixel := (MmsToPixelsHorizontal(PrinterSettings.WorkSheetWidthMms) -
    Max(MmsToPixelsHorizontal(Margin.Right), PrinterSettings.Gutter.Right)) -
    PixelLength;

  SetTab(0.0);
  WriteLine(PixelsToMmsHorizontal(StartPixel), Y, Text);
  SetTab(CurrentTab);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineColumn(ColumnNumber: Word; Y: Single; Text: string);
begin
  case Columns[ColumnNumber].FAlignment of
    taCenter: WriteLineColumnCenter(ColumnNumber, Y, Text);
    taRightJustify: WriteLineColumnRight(ColumnNumber, Y, Text);
    else
      WriteLineColumnLeft(ColumnNumber, Y, Text);
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineColumnLeft(ColumnNumber: Word; Y: Single; Text: string);
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Columns[ColumnNumber].FFont);
    WriteLine(Columns[ColumnNumber].FPosition, Y, Text);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineColumnCenter(ColumnNumber: Word; Y: Single; Text: string);
var
  PixelLength: Integer;
  StartPixel: Integer;
  Pixels: Integer;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Columns[ColumnNumber].FFont);
    PixelLength := Destination.TextWidth(Text);
    Pixels := MmsToPixelsHorizontal(Columns[ColumnNumber].FLength);
    StartPixel := (Pixels div 2) + MmsToPixelsHorizontal(Columns[ColumnNumber].FPosition) - (PixelLength div 2);

    SetTab(0.0);
    WriteLine(PixelsToMmsHorizontal(StartPixel), Y, Text);
    SetTab(CurrentTab);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteLineColumnRight(ColumnNumber: Word; Y: Single; Text: string);
var
  PixelLength: Word;
  StartPixel: Word;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Columns[ColumnNumber].FFont);
    PixelLength := Destination.TextWidth(Text);
    StartPixel := MmsToPixelsHorizontal(Columns[ColumnNumber].FPosition + Columns[ColumnNumber].FLength) - PixelLength;

    SetTab(0.0);
    WriteLine(PixelsToMmsHorizontal(StartPixel), Y, Text);
    SetTab(CurrentTab);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteRotatedLine(X, Y: Single; Text: string; Font: TFont; Angle: Integer); { Ecrit Text à la position X, Y (en MM). }

  procedure Imprimer(Canvas: TCanvas; X, Y: Integer; Text: string; Angle: Integer);
  var
    //    LastSize: Integer;
    l, t: Integer;
    LastBkMode: Integer;
    LFont: TLogFont;
    hOldFont, hNewFont: HFont;
  begin
    l := X;
    t := Y;
    if Assigned(OPreview) then
    begin
      Inc(l, PrinterSettings.Gutter.Left);
      Inc(t, PrinterSettings.Gutter.Top);
    end;
    LastBkMode := GetBkMode(Canvas.Handle);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    //    case OptionSize of
    //      1: begin
    GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LFont);
    LFont.lfEscapement := Angle;
    hNewFont := CreateFontIndirect(LFont);
    hOldFont := SelectObject(Canvas.Handle, hNewFont);
    Canvas.TextOut(l, t, CleanString(Text));
    hNewFont := SelectObject(Canvas.Font.Handle, hOldFont);
    //        end;
    //      else begin
    //          LastSize := Canvas.Font.Height; // plus précis que Size
    //          Canvas.Font.Height := MulDiv(LastSize, ResolutionEcran, ResolutionPrinterY);
    //          GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LFont);
    //          LFont.lfEscapement := Angle;
    //          hNewFont := CreateFontIndirect(LFont);
    //          hOldFont := SelectObject(Canvas.Handle, hNewFont);
    //          Canvas.TextOut(MulDiv(l, ResolutionEcran, ResolutionPrinterX),
    //            MulDiv(t, ResolutionEcran, ResolutionPrinterY),
    //            CleanString(Text));
    //          hNewFont := SelectObject(Canvas.Font.Handle, hOldFont);
    //          Canvas.Font.Height := LastSize;
    //        end;
    //    end;
    DeleteObject(hNewFont);
    SetBkMode(Canvas.Handle, LastBkMode);
  end;

var
  XPixels: Integer;
  YPixels: Integer;
begin
  SetFontInformation2(Font);

  if (X >= 0.0) then
    XPixels := MmsToPixelsHorizontal(X)
  else
    XPixels := FMargin.Left;
  if (XPixels < PrinterSettings.Gutter.Left) then XPixels := PrinterSettings.Gutter.Left;
  if (CurrentTab > 0.0) then Inc(XPixels, MmsToPixelsHorizontal(CurrentTab));

  if (Y < 0) and (Y <> -2.0) and (Y <> -1.0) then Exit;
  if (Y = -1.0) then
  begin
    LastYPosition := LastYPosition + Max(GetLineHeightMmsFont(PrevFont), GetLineHeightMms);
    if (FAutoPaging = True) then
      if (LastYPosition > PrinterSettings.WorkSheetLengthMms - Detail.Bottom) then NewPage;
    YPixels := MmsToPixelsVertical(LastYPosition);
  end
  else
  begin
    if (Y = -2.0) then
    begin
      if (FAutoPaging = True) then
        if (LastYPosition > PrinterSettings.WorkSheetLengthMms - Detail.Bottom) then NewPage;
      YPixels := MmsToPixelsVertical(LastYPosition);
    end
    else
    begin
      YPixels := MmsToPixelsVertical(Y);
      if (YPixels < PrinterSettings.Gutter.Top) then YPixels := PrinterSettings.Gutter.Top;
      if (YPixels > MmsToPixelsVertical(PrinterSettings.WorkSheetLengthMms) - PrinterSettings.Gutter.Bottom) then YPixels := MmsToPixelsVertical(PrinterSettings.WorkSheetLengthMms) - PrinterSettings.Gutter.Bottom;
      LastYPosition := PixelsToMmsVertical(YPixels);
    end;
  end;
  Imprimer(Destination, XPixels - PrinterSettings.Gutter.Left, YPixels - PrinterSettings.Gutter.Top, Text, Angle);
  PrevFont.Assign(Destination.Font);
end;
{================ Ecriture =====================================================}

{================ Dessin =======================================================}

procedure TPrintObject._DrawBox(XTop, YTop, XBottom, YBottom: Integer; LineWidth: Word; Shading: TColor);
var
  t, b, l, r: Integer;
begin
  Destination.Pen.Width := LineWidth;
  Destination.Brush.Color := Shading;

  t := YTop;
  b := YBottom;
  l := XTop;
  r := XBottom;
  if Assigned(OPreview) then
  begin
    Inc(t, PrinterSettings.Gutter.Top);
    Inc(b, PrinterSettings.Gutter.Bottom);
    Inc(l, PrinterSettings.Gutter.Left);
    Inc(r, PrinterSettings.Gutter.Right);
  end;

  //  case OptionSize of
  //    1:
  Destination.Rectangle(l, t, r, b);
  //    else Destination.Rectangle(MulDiv(l, ResolutionEcran, ResolutionPrinterX),
  //                               MulDiv(t, ResolutionEcran, ResolutionPrinterY),
  //                               MulDiv(r, ResolutionEcran, ResolutionPrinterX),
  //                               MulDiv(b, ResolutionEcran, ResolutionPrinterY));
  //  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject._PrintPic(dstX, dstY, dstWidth, dstHeight: Integer; Graphic: TGraphic);
var
  Info: PBitmapInfo;
  InfoSize, ImageSize: Cardinal;
  Image: TMemoryStream;
begin
  if Assigned(OPreview) then
  begin
    Inc(dstX, PrinterSettings.Gutter.Top);
    Inc(dstY, PrinterSettings.Gutter.Left);
  end;
  //  if OptionSize <> 1 then begin
  //    dstX := MulDiv(dstX, ResolutionEcran, ResolutionPrinterX);
  //    dstY := MulDiv(dstY, ResolutionEcran, ResolutionPrinterY);
  //    dstWidth := MulDiv(dstWidth, ResolutionEcran, ResolutionPrinterX);
  //    dstHeight := MulDiv(dstHeight, ResolutionEcran, ResolutionPrinterY);
  //  end;
  if Assigned(OPreview) then
  begin
    Destination.StretchDraw(Rect(dstX, dstY, dstX + dstWidth, dstY + dstHeight), Graphic);
    Exit;
  end;
  // not FPreview:
  with TBitmap.Create do
  try
    Height := Graphic.Height;
    Width := Graphic.Width;
    Palette := Graphic.Palette;
    SetStretchBltMode(Canvas.Handle, STRETCH_DELETESCANS);
    Canvas.Draw(0, 0, Graphic);
    Palette := Graphic.Palette;
    GetDIBSizes(Handle, InfoSize, ImageSize);
    GetMem(Info, InfoSize);
    try
      Image := TMemoryStream.Create;
      try
        Image.SetSize(ImageSize);
        GetDIB(Handle, Palette, Info^, Image.Memory^);
        with Info^.bmiHeader do
          StretchDIBits(Destination.Handle, dstX, dstY, dstWidth, dstHeight, 0, 0, biWidth, biHeight, Image.Memory, Info^, DIB_RGB_COLORS, SRCCOPY);
      finally
        Image.Free;
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
  finally
    Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.Draw(Left, Top, Value: Single; Graphic: TGraphic; DrawOption: TDrawOption = doByHeight);
begin
  if DrawOption = doByHeight then
    Draw(Left, Top, Value, Value * Graphic.Width / Graphic.Height, Graphic)
  else
    Draw(Left, Top, Value * Graphic.Height / Graphic.Width, Value, Graphic);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.Draw(Left, Top, Height, Width: Single; Graphic: TGraphic);
var
  l, t, h, w: Integer;
begin
  l := MmsToPixelsHorizontal(Left) - PrinterSettings.Gutter.Left;
  t := MmsToPixelsVertical(Top) - PrinterSettings.Gutter.Top;
  h := MmsToPixelsVertical(Height);
  w := MmsToPixelsHorizontal(Width);
  _PrintPic(l, t, w, h, Graphic);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.Draw(Left, Top: Single; Graphic: TGraphic);
var
  l, t: Integer;
begin
  l := MmsToPixelsHorizontal(Left) - PrinterSettings.Gutter.Left;
  t := MmsToPixelsVertical(Top) - PrinterSettings.Gutter.Top;
  _PrintPic(l, t, Graphic.Width, Graphic.Height, Graphic);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.DrawBox(XTop, YTop, XBottom, YBottom: Single; LineWidth: Word);
var
  BLinePixels, BColPixels, ELinePixels, EColPixels: Integer;
  LastBkMode: Integer;
begin
  BLinePixels := MmsToPixelsVertical(YTop) - PrinterSettings.Gutter.Top;
  ELinePixels := MmsToPixelsVertical(YBottom) - PrinterSettings.Gutter.Top;
  BColPixels := MmsToPixelsHorizontal(XTop) - PrinterSettings.Gutter.Left;
  EColPixels := MmsToPixelsHorizontal(XBottom) - PrinterSettings.Gutter.Left;
  LastBkMode := GetBkMode(Destination.Handle);
  SetBkMode(Destination.Handle, TRANSPARENT);
  _DrawBox(BColPixels, BLinePixels, EColPixels, ELinePixels, LineWidth, clWhite);
  SetBkMode(Destination.Handle, LastBkMode);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.DrawBoxShaded(XTop, YTop, XBottom, YBottom: Single; LineWidth: Word; Shading: TColor);
var
  BLinePixels, BColPixels, ELinePixels, EColPixels: Integer;
begin
  BLinePixels := MmsToPixelsVertical(YTop) - PrinterSettings.Gutter.Top;
  ELinePixels := MmsToPixelsVertical(YBottom) - PrinterSettings.Gutter.Top;
  BColPixels := MmsToPixelsHorizontal(XTop) - PrinterSettings.Gutter.Left;
  EColPixels := MmsToPixelsHorizontal(XBottom) - PrinterSettings.Gutter.Left;
  _DrawBox(BColPixels, BLinePixels, EColPixels, ELinePixels, LineWidth, Shading);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.DrawLine(TopX, TopY, BottomX, BottomY: Single; LineWidth: Word; Color: TColor);
var
  TopXPixels, BottomXPixels, TopYPixels, BottomYPixels: Integer;
  SavedColor: TColor;
begin
  TopXPixels := MmsToPixelsHorizontal(TopX);
  BottomXPixels := MmsToPixelsHorizontal(BottomX);
  TopYPixels := MmsToPixelsVertical(TopY);
  BottomYPixels := MmsToPixelsVertical(BottomY);

  if not Assigned(OPreview) then
  begin
    Dec(TopXPixels, PrinterSettings.Gutter.Left);
    Dec(BottomXPixels, PrinterSettings.Gutter.Left);
    Dec(TopYPixels, PrinterSettings.Gutter.Top);
    Dec(BottomYPixels, PrinterSettings.Gutter.Top);
  end;

  Destination.Pen.Width := LineWidth;
  SavedColor := Destination.Pen.Color;
  Destination.Pen.Color := Color;

  //  if OptionSize = 2 then begin
  //    TopXPixels := MulDiv(TopXPixels, ResolutionEcran, ResolutionPrinterX);
  //    TopYPixels := MulDiv(TopYPixels, ResolutionEcran, ResolutionPrinterY);
  //    BottomXPixels := MulDiv(BottomXPixels, ResolutionEcran, ResolutionPrinterX);
  //    BottomYPixels := MulDiv(BottomYPixels, ResolutionEcran, ResolutionPrinterY);
  //  end;

  Destination.MoveTo(TopXPixels, TopYPixels);
  Destination.LineTo(BottomXPixels, BottomYPixels);
  Destination.Pen.Color := SavedColor;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineWidth;
begin
  Result := Destination.Pen.Width;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetLineWidth(Width: Word);
begin
  Destination.Pen.Width := Width;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.StretchDraw(Left, Top, Width, Height: Single; Graphic: TGraphic);
var
  l, t, w, h: Integer;
begin
  if (Width = 0) and (Height = 0) then
  begin
    Draw(Top, Left, Graphic);
    Exit;
  end;

  t := MmsToPixelsVertical(Top) - PrinterSettings.Gutter.Top;
  l := MmsToPixelsHorizontal(Left) - PrinterSettings.Gutter.Left;
  w := MmsToPixelsVertical(Width);
  h := MmsToPixelsHorizontal(Height);
  if (Height = 0) then h := MulDiv(w, Graphic.Height, Graphic.Width);
  if (Width = 0) then w := MulDiv(h, Graphic.Width, Graphic.Height);

  _PrintPic(l, t, w, h, Graphic);
end;
{============== Dessin =========================================================}

{============== Impression =====================================================}

procedure TPrintObject.Abort;
var
  Cancel: Boolean;
begin
  CheckPrinting;
  Cancel := False;
  if Assigned(FBeforeAbort) then FBeforeAbort(Self, Cancel);
  if Cancel then Exit;
  FPrintingState := psAborting;
  if Assigned(FPreviewCanvas) then FreeAndNil(FPreviewCanvas);
  if Assigned(OPreview) then
  begin
    OPreview.Abort;
  end;
  FPrintingState := psNone;
  if Assigned(FAfterAbort) then FAfterAbort(Self);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.CalculateLineHeight;
begin
  Result := TextMetrics.tmHeight + TextMetrics.tmExternalLeading;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.Destination;
begin
  CheckPrinting;
  Result := FPreviewCanvas;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetColumnsPerLine;
var
  Pixels: Integer;
begin
  Pixels := PrinterSettings.PaperWidthPixels - PrinterSettings.Gutter.Left - PrinterSettings.Gutter.Right;
  Result := Pixels div Destination.TextWidth('B');
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetColumnsPerLineFont(Font: TFont): Integer;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetColumnsPerLine;
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineHeightPixels;
begin
  Result := CalculateLineHeight;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineHeightMms;
begin
  Result := PixelsToMmsVertical(GetLineHeightPixels);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineHeightMmsFont(FontName: string; FontSize: Word; FontStyle: TFontStyles): Single;
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Result := GetLineHeightMmsFont(Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineHeightMmsFont(Font: TFont): Single;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetLineHeightMms;
  finally
    RestoreCurrentfont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesInDetailArea;
begin
  Result := MmsToPixelsVertical(Detail.Top + Detail.Height) div CalculateLineHeight;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesInDetailAreaFont(Font: TFont): Word;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetLinesInDetailArea;
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesLeft;
begin
  Result := Trunc((Detail.Top + Detail.Height - LastYPosition) / GetLineHeightMms);
end;

function TPrintObject.GetHeightLeftMms: Single;
begin
  Result := Detail.Top + Detail.Height - LastYPosition;
end;

function TPrintObject.GetHeightLeftPixel: Integer;
begin
  Result := MmsToPixelsVertical(GetHeightLeftMms);
end;

{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesLeftFont(Font: TFont): Word;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetLinesLeft;
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesPerPage: Integer;
begin
  Result := (PrinterSettings.PaperLengthPixels - PrinterSettings.Gutter.Top - PrinterSettings.Gutter.Bottom) div CalculateLineHeight;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLinesPerPageFont(Font: TFont): Integer;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetLinesPerPage;
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineText1(var Start: Integer; Line: Word; Len: Single; Text: string; FontName: string; FontSize: Word; FontStyle: TFontStyles; var FinTexte: Boolean; TextOptions: TTextOptions = []): string;
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    Result := GetLineText2(Start, Line, Len, Text, Ft, FinTexte, TextOptions);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetLineText2(var Start: Integer; Line: Word; Len: Single; Text: string; Font: TFont; var FinTexte: Boolean; TextOptions: TTextOptions = []): string;

  function PosGetLineText(Debut: Integer; const Texte, AChercher: string): Integer;
  var
    Temp, P: PChar;
  begin
    if not Debut in [1..Length(Texte)] then
    begin
      Result := 0;
      Exit;
    end;
    Temp := @Texte[Debut];
    P := StrPos(Temp, PChar(AChercher));
    if P = nil then
      Result := 0
    else
      Result := P - Temp + Debut;
  end;

var
  txt: string;
  PosTotal, PosIntPrec, PosInt, PosEnt, l, ll, Decalage: Integer;
  Fin, lw: Boolean;
begin
  Fin := False;
  FinTexte := True;
  Result := '';
  if Start > Length(Text) then Exit;
  if Start < 0 then Start := 1;
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    PosIntPrec := Start - 1;
    PosTotal := 1;
    Decalage := 0;
    l := Pos(#10, text);
    while l > 0 do
    begin
      if (l <= PosIntPrec + 1) then Inc(Decalage);
      if (l < PosIntPrec + 1) then Dec(PosIntPrec);
      Delete(text, l, 1);
      l := Pos(#10, text);
    end;
    ll := MmsToPixelsHorizontal(Len);
    for l := 1 to Line do
    begin
      PosTotal := PosTotal + PosIntPrec;
      PosInt := 0;
      txt := Copy(Text, PosTotal, Length(Text) - PosTotal + 1);
      PosEnt := Pos(#13, txt);
      repeat
        PosIntPrec := PosInt;
        PosInt := PosGetLineText(PosIntPrec + 1, txt, ' ');
        lw := (Destination.TextWidth(Copy(text, PosTotal, PosInt - 1)) > ll);
      until (lw)
        or (PosInt = 0)
        or ((PosEnt < PosInt) and (PosEnt > 0));
      if not lw and (PosEnt < PosInt) and (PosEnt > 0) then
      begin
        PosInt := PosEnt + 1;
        if (l <> Line) then
        begin
          fin := False;
          PosIntPrec := PosEnt;
        end
        else
        begin
          fin := True;
          PosIntPrec := PosEnt - 1;
        end;
      end;
      if not lw and (PosInt = 0) then
      begin
        PosIntPrec := Length(txt);
        if (l < Line) then Exit;
      end;
    end;
    if toExact in TextOptions then
      while (Destination.TextWidth(Copy(Text, PosTotal, PosIntPrec)) > ll) and (PosIntPrec > 0) do
        Dec(PosIntPrec);

    Result := Copy(Text, PosTotal, PosIntPrec);
    Start := PosTotal + PosIntPrec + Decalage;
    FinTexte := Start >= Length(Text);
    if fin then Inc(Start);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetPageNumber;
begin
  case Assigned(OPreview) of
    True: Result := OPreview.Pages.Count;
    else
      Result := FPages.Count;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetPrinting: Boolean;
begin
  Result := GetPrintingEx([psStarting, psEnding]);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetPrintingEx(States: TPrintingStates = []): Boolean;
begin
  Result := FPrintingState in ([psPrinting] + States);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetTextWidthFont(Text: string; Font: TFont): Single;
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    Result := GetTextWidth(Text);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetTextWidth(Text: string): Single;
begin
  Result := PixelsToMmsHorizontal(Destination.TextWidth(Text));
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetTitre;
begin
  Result := FTitre;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.GetYPosition;
begin
  Result := LastYPosition;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.MmsToPixelsHorizontal(Milimeters: Single): Integer;
begin
  Result := Round(Milimeters * PrinterSettings.PixelsPerMmY);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.MmsToPixelsVertical(Milimeters: Single): Integer;
begin
  Result := Round(Milimeters * PrinterSettings.PixelsPerMmX);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.NewLines(Number: Single);
begin
  LastYPosition := LastYPosition + GetLineHeightMms * Number;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.NewLinesFont(Number: Word; Font: TFont);
begin
  SaveCurrentFont;
  try
    SetFontInformation2(Font);
    NewLines(Number);
  finally
    RestoreCurrentFont;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.NewPage;
var
  Cancel: Boolean;
begin
  CheckPrinting([psStarting]);
  Cancel := False;
  if Assigned(FBeforeNewPage) then FBeforeNewPage(Self, Cancel);
  if Cancel then Exit;
  if FPrintingState <> psStarting then
  begin
    SaveCurrentFont;
    try
      WriteHeader;
      WriteFooter;
      WritePageNumber;
      WriteDateTime;
      CreatePage;
    finally
      RestoreCurrentFont;
    end;
  end;
  LastYPosition := Detail.Top - GetLineHeightMms;
  SetTopOfPage;
  if Assigned(FAfterNewPage) then FAfterNewPage(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.NextLine;
begin
  LastYPosition := LastYPosition + GetLineHeightMms;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.NextLineFont(Font: TFont);
begin
  LastYPosition := LastYPosition + GetLineHeightMmsFont(Font);
end;
{-------------------------------------------------------------------------------}

function TPrintObject.PixelsToMmsHorizontal(Pixels: Integer): Single;
begin
  Result := Pixels / PrinterSettings.PixelsPerMmY;
end;
{-------------------------------------------------------------------------------}

function TPrintObject.PixelsToMmsVertical(Pixels: Integer): Single;
begin
  Result := Pixels / PrinterSettings.PixelsPerMmX;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.Quit; { 'Quit' doit être appelée pour valider l'impression }
var
  Cancel: Boolean;
begin
  CheckPrinting;
  Cancel := False;
  if Assigned(FBeforeQuit) then FBeforeQuit(Self, Cancel);
  if Cancel then Exit;
  FPrintingState := psEnding;
  WriteHeader;
  WriteFooter;
  WritePageNumber;
  WriteDateTime;
  if Assigned(FPreviewCanvas) then FreeAndNil(FPreviewCanvas);
  if Assigned(OPreview) then
  begin
    OPreview.Quit;
  end
  else
  begin
    PrintPages(FPages);
  end;
  FPrintingState := psNone;
  if Assigned(FAfterQuit) then FAfterQuit(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.RestoreCurrentFont;
begin
  if CurrentFont.Count < 1 then Exit;
  SetFontInformation2(CurrentFont[0].FFont);
  CurrentFont[0].Free;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SaveCurrentFont;
begin
  with CurrentFont.Add(Destination.Font) do
    SetIndex(0);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFontInformation1(FontName: string; FontSize: Word; FontStyle: TFontStyles);
var
  Ft: TFont;
begin
  Ft := TFont.Create;
  try
    Ft.Name := FontName;
    Ft.Size := FontSize;
    Ft.Style := FontStyle;
    SetFontInformation2(Ft);
  finally
    Ft.Free;
  end;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetFontInformation2(Font: TFont);
//var
//  LastSize: Integer;
begin
  Destination.Font.Assign(Font); // buggé: ne transmet pas l'Escapement !!!!
  // correction: pas buggé: TFont n'utilise pas l'Escapement
//  if FPreview then begin
//    LastSize := Destination.Font.Size;
//    Destination.Font.PixelsPerInch := ResolutionPrinterY;
//    Destination.Font.Size := LastSize;
//  end;
  Self.Font := Font;
  CalculateTextMetrics;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetTopOfPage;
begin
  LastYPosition := Detail.Top;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.SetYPosition(YPosition: Single);
begin
  LastYPosition := YPosition;
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.Start(Title: string); { doit TOUJOURS être appelée avant de commencer une impression }
var
  Cancel: Boolean;
begin
  if Printing then RaiseError(EPrinting);
  Cancel := False;
  if Assigned(FBeforeStart) then FBeforeStart(Self, Cancel);
  if Cancel then Exit;
  FPrintingState := psStarting;
  StartDateTimePrint := Now;
  Titre := Title;
  CreatePage;
  if Assigned(OPreview) then
  begin
    OPreview.SetHeightMM(PrinterSettings.WorkSheetLengthMms);
    OPreview.SetWidthMM(PrinterSettings.WorkSheetWidthMms);
    OPreview.SetCaption(Title);
    OPreview.Start;
  end;
  SetFontInformation2(FFont);
  CurrentTab := 0.0;
  LastYPosition := 0.0;
  NewPage;
  FPrintingState := psPrinting;
  if Assigned(FAfterStart) then FAfterStart(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteDateTime;
var
  Buffer: string;
  Temp: Boolean;
  Cancel: Boolean;
begin
  if not DateTime.Printed then Exit;
  Cancel := False;
  if Assigned(FBeforeDateTime) then FBeforeDateTime(Self, Cancel);
  if Cancel then Exit;
  if DateTime.DateType = dtCurrent then
    Buffer := FormatDateTime(DateTime.PrintFormat, Now)
  else
    Buffer := FormatDateTime(DateTime.PrintFormat, StartDateTimePrint);
  SaveCurrentFont;
  Temp := FAutoPaging;
  try
    SetFontInformation2(DateTime.Font);
    FAutoPaging := False;
    case DateTime.Alignment of
      taCenter: WriteLineCenter(DateTime.Position, Buffer);
      taRightJustify: WriteLineRight(DateTime.Position, Buffer);
      else
        WriteLine(Margin.Left, DateTime.Position, Buffer);
    end;
  finally
    FAutoPaging := Temp;
    RestoreCurrentFont;
  end;
  if Assigned(FAfterDateTime) then FAfterDateTime(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteFooter;
var
  i: Integer;
  ps: Single;
  Temp: Boolean;
  SavedColor: TColor;
  Cancel: Boolean;
begin
  if FFooter.Count = 0 then Exit;
  Cancel := False;
  if Assigned(FBeforeFooter) then FBeforeFooter(Self, Cancel);
  if Cancel then Exit;
  SavedColor := Destination.Brush.Color;
  SaveCurrentFont;
  Temp := FAutoPaging;
  try
    FAutoPaging := False;
    if (FooterCoordinates.Boxed = True) then
      if (FooterCoordinates.BackColor <> clWhite) then
        DrawBoxShaded(FooterCoordinates.FLeft, FooterCoordinates.FTop,
          FooterCoordinates.FLeft + FooterCoordinates.FWidth,
          FooterCoordinates.FTop + FooterCoordinates.FHeight,
          FooterCoordinates.FLineWidth, FooterCoordinates.FShading)
      else
        DrawBox(FooterCoordinates.FLeft, FooterCoordinates.FTop,
          FooterCoordinates.FLeft + FooterCoordinates.FWidth,
          FooterCoordinates.FTop + FooterCoordinates.FHeight,
          FooterCoordinates.FLineWidth);
    for i := 0 to FFooter.Count - 1 do
      with FFooter[i] do
        if (Length(FText) > 0) then
        begin
          SetFontInformation2(Font);
          ps := FPosition;
          if (ps <> -1) and (ps <> -2) then ps := PrinterSettings.WorkSheetLengthMms - FooterCoordinates.Bottom - ps;
          case Alignment of
            taCenter: WriteLineCenter(ps, FText);
            taRightJustify: WriteLineRight(ps, FText);
            else
              WriteLine(FooterCoordinates.FLeft, ps, FText);
          end;
        end;
  finally
    Destination.Brush.Color := SavedColor;
    FAutoPaging := Temp;
    RestoreCurrentFont;
  end;
  if Assigned(FAfterFooter) then FAfterFooter(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WriteHeader;
var
  i: Integer;
  ps: Single;
  SavedColor: TColor;
  Cancel: Boolean;
begin
  if FHeader.Count = 0 then Exit;
  Cancel := False;
  if Assigned(FBeforeHeader) then FBeforeHeader(Self, Cancel);
  if Cancel then Exit;
  SavedColor := Destination.Brush.Color;
  SaveCurrentFont;
  try
    if (HeaderCoordinates.Boxed = True) then
      if (HeaderCoordinates.BackColor <> clWhite) then
        DrawBoxShaded(HeaderCoordinates.FLeft, HeaderCoordinates.FTop,
          HeaderCoordinates.FLeft + HeaderCoordinates.FWidth,
          HeaderCoordinates.FTop + HeaderCoordinates.FHeight,
          HeaderCoordinates.FLineWidth, HeaderCoordinates.FShading)
      else
        DrawBox(HeaderCoordinates.FLeft, HeaderCoordinates.FTop,
          HeaderCoordinates.FLeft + HeaderCoordinates.FWidth,
          HeaderCoordinates.FTop + HeaderCoordinates.FHeight,
          HeaderCoordinates.FLineWidth);
    for i := 0 to FHeader.Count - 1 do
      with FHeader[i] do
        if (Length(Text) > 0) then
        begin
          SetFontInformation2(Font);
          ps := FPosition;
          if (ps <> -1) and (ps <> -2) then ps := ps + HeaderCoordinates.Top;
          case Alignment of
            taCenter: WriteLineCenter(ps, FText);
            taRightJustify: WriteLineRight(ps, FText);
            else
              WriteLine(HeaderCoordinates.FLeft, ps, FText);
          end;
        end;
  finally
    Destination.Brush.Color := SavedColor;
    RestoreCurrentFont;
  end;
  if Assigned(FAfterHeader) then FAfterHeader(Self);
end;
{-------------------------------------------------------------------------------}

procedure TPrintObject.WritePageNumber;
var
  Buffer: string;
  Temp: Boolean;
  Cancel: Boolean;
begin
  if not PageNumber.Printed then Exit;
  Cancel := False;
  if Assigned(FBeforePageNumber) then FBeforePageNumber(Self, Cancel);
  if Cancel then Exit;
  Buffer := Format(PageNumber.FText, [GetPageNumber]);
  SaveCurrentFont;
  Temp := FAutoPaging;
  try
    FAutoPaging := False;
    SetFontInformation2(PageNumber.Font);
    case PageNumber.Alignment of
      taCenter: WriteLineCenter(PageNumber.Position, Buffer);
      taRightJustify: WriteLineRight(PageNumber.Position, Buffer);
      else
        WriteLine(Margin.Left, PageNumber.Position, Buffer);
    end;
  finally FAutoPaging := Temp;
    RestoreCurrentFont;
  end;
  if Assigned(FAfterPageNumber) then FAfterPageNumber(Self);
end;
{=============== Impression ====================================================}
{############### TPrintObject ##################################################}

{###############################################################################}

procedure RaiseError;
begin
  raise EPrintObject.Create(Msg);
end;
{-------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Tetram', [TPrintObject]);
end;
{###############################################################################}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////pompage QuickReport////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TPrinterSettings.Create(AOwner: TPrintObject);
begin
  inherited Create;
  Owner := AOwner;
  FPixelsPerX := 1;
  FPixelsPerY := 1;
  FPixelsPerMmX := 1;
  FPixelsPerMmY := 1;
  GetMem(FDevice, 128);
  GetMem(FDriver, 128);
  GetMem(FPort, 128);
  FPaperSize := A4;
  //  GetPrinterSettings;
end;

destructor TPrinterSettings.Destroy;
begin
  FreeMem(FDevice, 128);
  FreeMem(FDriver, 128);
  FreeMem(FPort, 128);
  inherited Destroy;
end;

function TPrinterSettings.GetCopies: integer;
begin
  Result := FCopies;
end;

function TPrinterSettings.GetDriver: string;
begin
  Result := StrPas(FDriver);
end;

function TPrinterSettings.GetDuplex: boolean;
begin
  Result := FDuplex;
end;

function TPrinterSettings.GetMaxExtentX: integer;
begin
  Result := FMaxExtentX;
end;

function TPrinterSettings.GetMaxExtentY: integer;
begin
  Result := FMaxExtentY;
end;

function TPrinterSettings.GetMinExtentX: integer;
begin
  Result := FMinExtentX;
end;

function TPrinterSettings.GetMinExtentY: integer;
begin
  Result := FMinExtentY;
end;

function TPrinterSettings.GetOrientation: TPrinterOrientation;
begin
  Result := FOrientation;
end;

function TPrinterSettings.GetOutputBin: TQRBin;
begin
  Result := FOutputBin;
end;

function TPrinterSettings.GetPaperSize: TQRPaperSize;
begin
  Result := FPaperSize;
end;

function TPrinterSettings.GetPaperSizeSupported(PaperSize: TQRPaperSize): Boolean;
begin
  Result := FPaperSizes[PaperSize];
end;

function TPrinterSettings.GetPaperWidthMms: Single;
begin
  if (PaperSize <> Custom) and (PaperSize <> Default) then
    Result := cQRPaperSizeMetrics[PaperSize, 0]
  else
    Result := FPaperWidthMms;
end;

function TPrinterSettings.GetPaperLengthMms: Single;
begin
  if (PaperSize <> Custom) and (PaperSize <> Default) then
    Result := cQRPaperSizeMetrics[PaperSize, 1]
  else
    Result := FPaperLengthMms;
end;

function TPrinterSettings.GetPixelsPerX: Integer;
begin
  Result := FPixelsPerX;
end;

function TPrinterSettings.GetPixelsPerY: Integer;
begin
  Result := FPixelsPerY;
end;

function TPrinterSettings.GetPort: string;
begin
  Result := StrPas(FPort);
end;

function TPrinterSettings.GetTopOffset: Integer;
begin
  Result := FTopOffset;
end;

function TPrinterSettings.GetLeftOffset: Integer;
begin
  Result := FLeftOffset;
end;

function TPrinterSettings.GetPrinter: TPrinter;
begin
  Result := Printer;
end;

function TPrinterSettings.GetTitle: string;
begin
  Result := FTitle;
end;

function TPrinterSettings.Supported(Setting: DWORD): Boolean;
begin
  if Assigned(UsedPrinter) then
    Supported := (DevMode^.dmFields and Setting) = Setting
  else
    Supported := False;
end;

procedure TPrinterSettings.SetField(aField: DWORD);
begin
  DevMode^.dmFields := DevMode^.dmFields or aField;
end;

procedure TPrinterSettings.GetPrinterSettings;

  procedure GPrinter;
  var
    Driver_info_2: pDriverinfo2;
    Retrieved: DWORD;
    hPrinter: THandle;
  begin
    UsedPrinter.GetPrinter(FDevice, FDriver, FPort, DeviceMode);
    if DeviceMode = 0 then UsedPrinter.GetPrinter(FDevice, FDriver, FPort, DeviceMode);
    OpenPrinter(FDevice, hPrinter, nil);
    GetMem(Driver_info_2, 255);
    GetPrinterDriver(hPrinter, nil, 2, Driver_info_2, 255, Retrieved);
    StrLCopy(FDriver, PChar(ExtractFileName(StrPas(Driver_info_2^.PDriverPath)) + #0), 63);
    FreeMem(Driver_info_2, 255);
    DevMode := GlobalLock(DeviceMode);
  end;

  procedure GCopies; { Number of copies }
  begin
    if Supported(dm_copies) then
      FCopies := DevMode^.dmCopies
    else
      FCopies := 1;
  end;

  procedure GBin; { Paper bin }
  var
    aBin: Integer;
    I: TQRBin;
  begin
    FOutputBin := First;
    if Supported(dm_defaultsource) then
    begin
      aBin := DevMode^.dmDefaultSource;
      for I := First to Last do
      begin
        if cQRBinTranslate[I] = aBin then
        begin
          FOutputBin := I;
          Exit;
        end
      end
    end
  end;

  procedure GDuplex; { Duplex }
  begin
    if Supported(dm_duplex) and (DevMode^.dmDuplex <> dmdup_simplex) then
      FDuplex := True
    else
      FDuplex := False;
  end;

  procedure GPixelsPer; { Horizontal and Vertical pixels per inch }
  begin
    FPixelsPerX := GetDeviceCaps(UsedPrinter.Handle, LOGPIXELSX);
    FPixelsPerY := GetDeviceCaps(UsedPrinter.Handle, LOGPIXELSY);
    FPixelsPerMmY := FPixelsPerY / 25.4;
    FPixelsPerMmX := FPixelsPerX / 25.4;
  end;

  procedure GOffset; { Top left printing offset (waste) }
  var
    PrintOffset: TPoint;
    EscapeFunc: Word;
  begin
    EscapeFunc := GetPrintingOffset;
    if Escape(UsedPrinter.Handle, QueryEscSupport, SizeOf(EscapeFunc), @EscapeFunc, nil) <> 0 then
    begin
      Escape(UsedPrinter.Handle, GetPrintingOffset, 0, nil, @PrintOffset);
      FLeftOffset := round(PrintOffset.x / PixelsPerX * 25.4);
      FTopOffset := round(PrintOffset.y / PixelsPerY * 25.4);
    end
    else
    begin
      FLeftOffset := 0;
      FTopOffset := 0;
    end;
  end;

  procedure GOrientation;
  begin
    if Supported(dm_orientation) and (DevMode^.dmOrientation = dmorient_landscape) then
      FOrientation := poLandscape
    else
      FOrientation := poPortrait;
  end;

  procedure GPaperSize;
  var
    aSize: Integer;
    I: TQRPaperSize;
  begin
    FPaperSize := Default;
    if Supported(dm_papersize) then
    begin
      aSize := DevMode^.dmPaperSize;
      for I := Default to Custom do
      begin
        if aSize = cQRPaperTranslate[I] then
        begin
          FPaperSize := I;
          Exit;
        end
      end
    end
  end;

  procedure GPaperDim;
  var
    PSize: TPoint;
    EscapeFunc: Word;
  begin
    EscapeFunc := GetPhysPageSize;
    if Escape(UsedPrinter.Handle, QueryEscSupport, SizeOf(EscapeFunc), @EscapeFunc, nil) <> 0 then
    begin
      Escape(UsedPrinter.Handle, GetPhysPageSize, 0, nil, @PSize);
      FPaperWidthMms := PSize.X / PixelsPerX * 25.4;
      FPaperWidthPixels := Owner.MmsToPixelsHorizontal(FPaperWidthMms);
      FPaperLengthMms := PSize.Y / PixelsPerY * 25.4;
      FPaperLengthPixels := Owner.MmsToPixelsVertical(FPaperLengthMms);
    end
    else
    begin
      FPaperWidthMms := 0;
      FPaperWidthPixels := Owner.MmsToPixelsHorizontal(FPaperWidthMms);
      FPaperLengthMms := 0;
      FPaperLengthPixels := Owner.MmsToPixelsVertical(FPaperLengthMms);
    end
  end;

  procedure GPaperSizes;
  var
    DCResult: array[0..255] of Word;
    I: Integer;
    J: TQRPaperSize;
    Count: Integer;
  begin
    FillChar(DCResult, SizeOf(DCResult), #0);
    FillChar(FPaperSizes, SizeOf(FPaperSizes), #0);
    Count := winspool.DeviceCapabilities(FDevice, FPort, DC_PAPERS, @DCResult, DevMode);
    for I := 0 to Count - 1 do
    begin
      for J := Default to Custom do
      begin
        if cQRPaperTranslate[J] = DCResult[I] then
        begin
          FPaperSizes[J] := true;
          break;
        end
      end
    end
  end;

  procedure GGutter;
  begin
    Gutter.Left := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    Gutter.Top := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
    Gutter.Right := PaperWidthPixels - Gutter.Left - GetDeviceCaps(UsedPrinter.Handle, HorzRes);
    Gutter.Bottom := PaperLengthPixels - Gutter.Top - GetDeviceCaps(UsedPrinter.Handle, VertRes);
  end;

begin
  if Assigned(UsedPrinter) then
  begin
    GPrinter;
    GPixelsPer;
    GCopies;
    GBin;
    GDuplex;
    GOffset;
    GOrientation;
    GPaperSize;
    GPaperDim;
    GPaperSizes;
    GGutter;
    GlobalUnlock(DeviceMode);
  end
end;

procedure TPrinterSettings.ApplySettings;
begin
  UsedPrinter.GetPrinter(FDevice, FDriver, FPort, DeviceMode);
  DevMode := GlobalLock(DeviceMode);
  if PaperSize = Custom then
  begin
    SetField(dm_paperlength);
    DevMode^.dmPaperLength := Round(PaperLengthMms * 10);
    SetField(dm_paperwidth);
    DevMode^.dmPaperWidth := Round(PaperWidthMms * 10);
  end;

  if Supported(dm_duplex) and FDuplex then
  begin
    SetField(dm_duplex);
    DevMode^.dmDuplex := dmdup_horizontal;
  end;
  if Supported(dm_PaperSize) and (PaperSize <> Default) then
  begin
    SetField(dm_papersize);
    DevMode^.dmPaperSize := cQRPaperTranslate[PaperSize];
  end;

  if Supported(dm_copies) then
  begin
    SetField(dm_copies);
    DevMode^.dmCopies := FCopies;
  end;

  if Supported(dm_defaultsource) then
  begin
    SetField(dm_defaultsource);
    DevMode^.dmDefaultSource := (DevMode^.dmDefaultSource and 256) or cQRBinTranslate[OutputBin];
  end;

  if Supported(dm_orientation) then
  begin
    SetField(dm_orientation);
    if Orientation = poPortrait then
      DevMode^.dmOrientation := dmorient_portrait
    else
      DevMode^.dmOrientation := dmorient_landscape;
  end;
  UsedPrinter.SetPrinter(FDevice, FDriver, FPort, DeviceMode);
  GlobalUnlock(DeviceMode);
end;

procedure TPrinterSettings.SetCopies(Value: integer);
begin
  if not Supported(dm_copies) then Exit;
  FCopies := Value;
  ApplySettings;
end;

procedure TPrinterSettings.SetDuplex(Value: boolean);
begin
  if not Supported(dm_duplex) then Exit;
  FDuplex := Value;
  ApplySettings;
end;

procedure TPrinterSettings.SetOrientation(Value: TPrinterOrientation);
begin
  if not Supported(dm_orientation) then Exit;
  FOrientation := Value;
  ApplySettings;
end;

procedure TPrinterSettings.SetOutputBin(Value: TQRBin);
begin
  if not Supported(dm_defaultsource) then Exit;
  FOutputBin := Value;
  ApplySettings;
end;

procedure TPrinterSettings.SetPaperSize(Value: TQRPaperSize);
begin
  if PaperSizeSupported[Value] then
    FPaperSize := Value
  else if (Value = Default) then
    FPaperSize := Default;
  ApplySettings;
end;

procedure TPrinterSettings.SetPaperLengthMms(Value: Single);
begin
  if PaperSize = Custom then FPaperLengthMms := Value;
  FPaperLengthPixels := Owner.MmsToPixelsVertical(FPaperLengthMms);
  ApplySettings;
end;

procedure TPrinterSettings.SetPaperWidthMms(Value: Single);
begin
  if PaperSize = Custom then FPaperWidthMms := Value;
  FPaperWidthPixels := Owner.MmsToPixelsHorizontal(FPaperWidthMms);
  ApplySettings;
end;

procedure TPrinterSettings.SetPrinter(Value: TPrinter);
begin
  UsedPrinter := Value;
  if Assigned(UsedPrinter) and (UsedPrinter.Printers.Count > 0) then
    GetPrinterSettings;
end;

procedure TPrinterSettings.SetTitle(Value: string);
begin
  FTitle := Value;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////
//constructor TPageInformations.Create(AParent : TPrintObject);
//begin
//  inherited Create;
//  Owner := AParent;
//  FMarginMm := TMarginsMms.Create(Self.Owner);
//  BottomMargin := 0;
////  if LocalMeasureInches then
////  begin
////    Units := Inches;
//    PaperSize := Letter;
//    TopMargin := 0.5;
//    BottomMargin := 0.5;
//    RightMargin := 0.5;
//    LeftMargin := 0.5;
////  end else
//  begin
////    Units := MM;
//    PaperSize := A4;
//    TopMargin := 10;
//    BottomMargin := 10;
//    LeftMargin := 10;
//    RightMargin := 10;
//  end;
//end;
//
//destructor TPageInformations.Destroy;
//begin
//  FMarginMm.Free;
//  inherited;
//end;
//
//function TPageInformations.GetValue(Index : integer): extended;
//begin
//  Result := 0;
//  case index of
//    0 : result := FBottomMargin;
//    1 : result := FHeight;
//    2 : result := FTopMargin;
//    3 : result := FWidth;
//    4 : result := FLeftMargin;
//    5 : result := FRightMargin;
//  end
//end;
//
//function TPageInformations.GetPaperSize : TQRPaperSize;
//begin
//  Result := FPaperSize;
//end;
//
//procedure TPageInformations.SetOrientation(Value : TPrinterOrientation);
//begin
//  FOrientation := Value;
//  PaperSize := PaperSize;
//end;
//
////procedure TPageInformations.SetParentSizes;
////begin
////  if not ParentUpdating and assigned(Parent) then
////  begin
////    ParentUpdating := true;
////    Parent.Width := round(LoadUnit(FWidth, Pixels, true));
////    Parent.Height := round(LoadUnit(FLength, Pixels, false));
////    ParentUpdating := false;
////  end;
////end;
//
//procedure TPageInformations.SetValue(Index : integer; Value : extended);
//begin
////  case index of
////    0 : begin
////          FBottomMargin := SaveUnit(Value, Units, false);
////          SetParentSizes;
////          Parent.SetBandValues;
////        end;
////    1 : if PaperSize=Custom then
////        begin
////          FLength := SaveUnit(Value, Units, false);
////          SetParentSizes;
////          Parent.SetBandValues;
////        end;
////    2 : begin
////          FTopMargin := SaveUnit(Value, Units, false);
////          Parent.SetBandValues;
////          Parent.Invalidate;
////        end;
////    3 : if PaperSize=Custom then
////        begin
////          FWidth := SaveUnit(Value, Units, true);
////          SetParentSizes;
////          Parent.SetBandValues;
////        end;
////    4 : begin
////          FLeftMargin := SaveUnit(Value, Units, true);
////          Parent.SetBandValues;
////          Parent.Invalidate;
////        end;
////    5 : begin
////          FRightMargin := SaveUnit(Value, Units, true);
////          Parent.SetBandValues;
////          Parent.Invalidate;
////        end;
////    6 : begin
////          FColumnSpace := SaveUnit(Value, Units, true);
////          Parent.SetBandValues;
////          Parent.Invalidate;
////        end;
////  end;
//end;
//
//procedure TPageInformations.SetPaperSize(Value : TQRPaperSize);
//begin
//  if (Value <> Default) and (Value <> Custom) then
//  begin
////    SetPixels;
////    Units := MM;
//    PaperSize := Custom;
//    if Orientation = poPortrait then
//    begin
//      Width := cQRPaperSizeMetrics[Value, 0];
//      Height := cQRPaperSizeMetrics[Value, 1];
//    end else
//    begin
//      Width := cQRPaperSizeMetrics[Value, 1];
//      Height := cQRPaperSizeMetrics[Value, 0];
//    end;
////    RestoreUnit;
////    Parent.SetBandValues;
//  end;
//  FPaperSize := Value;
//end;
//
//procedure TPageInformations.CalculateMeasurements;
//begin
////  PixelsPerMmVertical := ResolutionPrinterY / 25.4;
////  PixelsPerMmHorizontal := ResolutionPrinterX / 25.4;
//
////  Owner.Gutter.Left := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
////  Owner.Gutter.Top := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
//
//  WidthPixels := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
//  HeightPixels := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
//  Width := WidthPixels / PixelsPerMmHorizontal;
//  Height := HeightPixels / PixelsPerMmVertical;
//
////  Owner.Gutter.Right := WidthPixels - Owner.Gutter.Left - GetDeviceCaps(Printer.Handle, HorzRes);
////  Owner.Gutter.Bottom := HeightPixels - Owner.Gutter.Top - GetDeviceCaps(Printer.Handle, VertRes);
//
////  SetMargins(Margin.Top, Margin.Bottom, Margin.Left, Margin.Right);
//
////  if FHeaderCoordinates.FTop < PixelsToMmsVertical(Gutter.Top) then FHeaderCoordinates.Top := PixelsToMmsVertical(Gutter.Top);
////  if FHeaderCoordinates.FBottom < PixelsToMmsVertical(Gutter.Bottom) then FHeaderCoordinates.Bottom := PixelsToMmsVertical(Gutter.Bottom);
////  if FHeaderCoordinates.FLeft < PixelsToMmsHorizontal(Gutter.Left) then FHeaderCoordinates.Left := PixelsToMmsHorizontal(Gutter.Left);
////  if FHeaderCoordinates.FRight < PixelsToMmsHorizontal(Gutter.Right) then FHeaderCoordinates.Right := PixelsToMmsHorizontal(Gutter.Right);
//
////  if FFooterCoordinates.FTop < PixelsToMmsVertical(Gutter.Top) then FFooterCoordinates.Top := PixelsToMmsVertical(Gutter.Top);
////  if FFooterCoordinates.FBottom < PixelsToMmsVertical(Gutter.Bottom) then FFooterCoordinates.Bottom := PixelsToMmsVertical(Gutter.Bottom);
////  if FFooterCoordinates.FLeft < PixelsToMmsHorizontal(Gutter.Left) then FFooterCoordinates.Left := PixelsToMmsHorizontal(Gutter.Left);
////  if FFooterCoordinates.FRight < PixelsToMmsHorizontal(Gutter.Right) then FFooterCoordinates.Right := PixelsToMmsHorizontal(Gutter.Right);
//end;
{-------------------------------------------------------------------------------}
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TPrintObject.PrintPages(Pages: TList<TGraphic>);
var
  PrintDialog: TPrintDialog;
  PrinterPhysicalOffsetX, PrinterPhysicalOffsetY: Cardinal;
  I, J: Integer;
  StartPage, EndPage: Integer;
begin
  PrintDialog := TPrintDialog.Create(nil);
  try
    PrintDialog.Copies := 1;
    PrintDialog.Options := PrintDialog.Options + [poPageNums];
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := Pages.Count;

    if PrintDialog.Execute then
    begin
      PrinterSettings.UsedPrinter.Title := Titre;
      PrinterPhysicalOffsetX := GetDeviceCaps(PrinterSettings.UsedPrinter.Handle, PHYSICALOFFSETX);
      PrinterPhysicalOffsetY := GetDeviceCaps(PrinterSettings.UsedPrinter.Handle, PHYSICALOFFSETY);

      if PrintDialog.PrintRange = prAllPages then
      begin
        StartPage := 0;
        EndPage := Pred(Pages.Count);
      end
      else
      begin
        StartPage := PrintDialog.FromPage - 1;
        EndPage := PrintDialog.ToPage - 1;
      end;

      if PrintDialog.Collate then // Range * Copies
      begin
        if StartPage > EndPage then
        begin
          // print backwards
          for I := 0 to PrintDialog.Copies - 1 do
            for J := StartPage downto EndPage do
            begin
              if PrinterSettings.UsedPrinter.Aborted then
              begin
                if PrinterSettings.UsedPrinter.Printing then
                  PrinterSettings.UsedPrinter.EndDoc;
                Exit;
              end;
              if (J = StartPage) and (I = 0) then
                PrinterSettings.UsedPrinter.BeginDoc
              else
                PrinterSettings.UsedPrinter.NewPage;
              PrinterSettings.UsedPrinter.Canvas.Draw(-PrinterPhysicalOffsetX, -PrinterPhysicalOffsetY, Pages[J]);
            end;
        end
        else
        begin
          for I := 0 to PrintDialog.Copies - 1 do
            for J := StartPage to EndPage do
            begin
              if PrinterSettings.UsedPrinter.Aborted then
              begin
                if PrinterSettings.UsedPrinter.Printing then
                  PrinterSettings.UsedPrinter.EndDoc;
                Exit;
              end;
              if (J = StartPage) and (I = 0) then
                PrinterSettings.UsedPrinter.BeginDoc
              else
                PrinterSettings.UsedPrinter.NewPage;
              PrinterSettings.UsedPrinter.Canvas.Draw(-PrinterPhysicalOffsetX, -PrinterPhysicalOffsetY, Pages[J]);
            end;
        end;
      end
      else // Page * Copies
      begin
        if StartPage > EndPage then
        begin
          // print backwards
          for J := StartPage downto EndPage do
            for I := 0 to PrintDialog.Copies - 1 do
            begin
              if PrinterSettings.UsedPrinter.Aborted then
              begin
                if PrinterSettings.UsedPrinter.Printing then
                  PrinterSettings.UsedPrinter.EndDoc;
                Exit;
              end;
              if (J = StartPage) and (I = 0) then
                PrinterSettings.UsedPrinter.BeginDoc
              else
                PrinterSettings.UsedPrinter.NewPage;
              PrinterSettings.UsedPrinter.Canvas.Draw(-PrinterPhysicalOffsetX, -PrinterPhysicalOffsetY, Pages[J]);
            end;
        end
        else
        begin
          for J := StartPage to EndPage do
            for I := 0 to PrintDialog.Copies - 1 do
            begin
              if PrinterSettings.UsedPrinter.Aborted then
              begin
                if PrinterSettings.UsedPrinter.Printing then
                  PrinterSettings.UsedPrinter.EndDoc;
                Exit;
              end;
              if (J = StartPage) and (I = 0) then
                PrinterSettings.UsedPrinter.BeginDoc
              else
                PrinterSettings.UsedPrinter.NewPage;
              PrinterSettings.UsedPrinter.Canvas.Draw(-PrinterPhysicalOffsetX, -PrinterPhysicalOffsetY, Pages[J]);
            end;
        end;
      end;
      if PrinterSettings.UsedPrinter.Printing then
        PrinterSettings.UsedPrinter.EndDoc;
    end;
  finally
    PrintDialog.Free;
  end;
end;

procedure TPrintObject.SetTitre(const Value: string);
begin
  FTitre := Value;
end;

end.

