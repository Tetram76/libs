unit GzFileGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, GzGrids, GzFileList, GzSumInfo, Graphics;

type
  TFileGrid = class;

  TFormatContentEvent = procedure(Sender : TObject; AProperty : TCustomFileProperty; var AValue : string) of object;
  TFormatFileNameEvent = procedure(Sender : TObject; AItem : TCustomFileInformation; var AValue : string) of object;
  TFileLockedEvent = procedure(Sender : TObject; const AFileName : TFileName; var ShouldRetry : Boolean) of object;

  TFileColumnItem = class(TCollectionItem)
  private
    FPropertyName: string;
    FAlignment: TAlignment;
    FParent : TFileGrid;
    function GetParent: TFileGrid;
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure SetPropertyName(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
  protected
    function GetDisplayName: string; override;
    property Parent : TFileGrid read GetParent;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property PropertyName : string read FPropertyName write SetPropertyName;
    property Width : Integer read GetWidth write SetWidth;
    property Alignment : TAlignment read FAlignment write SetAlignment;
  end;

  TFileColumnItemClass = class of TFileColumnItem;

  TFileColumns = class(TCollection)
  private
    FParent: TFileGrid;
    function GetColumnItem(Index: Integer): TFileColumnItem;
    procedure SetColumnItem(Index: Integer; const Value: TFileColumnItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AParent: TFileGrid; AItemClass: TFileColumnItemClass);
    function Add: TFileColumnItem;
    property Parent: TFileGrid read FParent;
    property Items[Index: Integer]: TFileColumnItem read GetColumnItem write SetColumnItem; default;

    procedure Edit(CheckTitle : Boolean = False);
    procedure CheckColumnsParams;
  end;

  TFileChangeEvent = procedure (Sender : TObject; NewFile : TCustomFileInformation; var CanChange : Boolean) of object;
  TFilterItemEvent = procedure (Sender : TObject; AItem : TCustomFileInformation; var AcceptItem : Boolean) of object;

  TBeforeDrawFileNameEvent = procedure (Sender : TObject; ACanvas : TCanvas; AItem : TCustomFileInformation) of object;
  TBeforeDrawColumnEvent = procedure (Sender : TObject; ACanvas : TCanvas; AItem : TCustomFileInformation; AProperty : TCustomFileProperty) of object;

  TFileGrid = class(TGzCustomGrid)
  private
    { Déclarations privées }
    FTitles : TStringList;
    FNeedInvalidate : Boolean;
    FSelStart : Integer;
    FIgnoreSelClick : Boolean;
    FList,
    FFilteredList,
    FSelectedList : TThreadedFileList;
    FIcon: TIcon;
    FFileColumns: TFileColumns;
    FPropertyCount : Integer;
    FOnFormat: TFormatContentEvent;
    FOnFileChange: TFileChangeEvent;
    FPotentielSortPt : TPoint;
    FPotentialSortCol : Integer;
    FOnListChange: TNotifyEvent;
    FOnFileLocked: TFileLockedEvent;
    FOnAfterFileLocked: TNotifyEvent;
    FOnFilterItem: TFilterItemEvent;
    FFiltered: Boolean;
    FMultiSelection: Boolean;
    FBeforeDrawColumn: TBeforeDrawColumnEvent;
    FBeforeDrawFileName: TBeforeDrawFileNameEvent;
    FFileNameColumn: TFileColumnItem;
    FLineSelection: Boolean;
    FOnFormatFileName: TFormatFileNameEvent;
    FFileCountLimit: Integer;
    FTrueFileCount: Integer;
    procedure CheckParams;
    procedure SetDirectory(const Value: string);
    function GetFileClass: TFileClass;
    procedure SetFileClass(const Value: TFileClass);
    procedure SetFileColumns(const Value: TFileColumns);
    procedure CheckColCount;
    procedure CheckRowCount;
    procedure CheckRowHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetFileName: string;
    function GetSelected: TCustomFileInformation;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
    function GetDirectory: string;
    procedure SetFiltered(const Value: Boolean);
    function GetFilteredList: TThreadedFileList;
    procedure SetMultiSelection(const Value: Boolean);
    procedure SetSelected(const Value: TCustomFileInformation);
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(const Value: Integer);
    procedure InvalidateColonneFilename;
    procedure SetFileName(const Value: string);
    function GetSortDescending: Boolean;
    function GetSortIndex: Integer;
    procedure SetSortDescending(const Value: Boolean);
    procedure SetSortIndex(const Value: Integer);
    procedure SetLineSelection(const Value: Boolean);
  protected
    { Déclarations protégées }
    procedure Paint; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure ListChange(Sender : TObject);
    procedure FilteredListChange(Sender : TObject);
    procedure SelectedListChange(Sender: TObject);
    procedure WndProc(var Message: TMessage); override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    function SelectCell(ACol, ARow: Longint): Boolean; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoFormat(AProperty : TCustomFileProperty; var AValue : string);
    procedure DoFormatFileName(AItem : TCustomFileInformation; var AValue : string);

    function BeginColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint) : Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    function EndColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean; override;
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure CreateWnd; override;

    property FileClass : TFileClass read GetFileClass write SetFileClass;

    property FileName : string read GetFileName write SetFileName;
    property FileNameColumn : TFileColumnItem read FFileNameColumn;
    property Selected : TCustomFileInformation read GetSelected write SetSelected;
    property SelectedIndex : Integer read GetSelectedIndex write SetSelectedIndex;

    property Files : TThreadedFileList read FList;
    property FilteredFiles : TThreadedFileList read GetFilteredList;
    property SelectedFiles : TThreadedFileList read FSelectedList;

    property SortIndex : Integer read GetSortIndex write SetSortIndex;
    property SortDescending : Boolean read GetSortDescending write SetSortDescending;
    procedure Sort;

    property FileCountLimit : Integer read FFileCountLimit write FFileCountLimit;
    property TrueFileCount : Integer read FTrueFileCount;

    procedure Filter(AFilterEvent : TFilterItemEvent = nil);

    procedure InvalidateFile(AFileName : string); overload;
    procedure InvalidateFile(AItem : TCustomFileInformation); overload;
  published
    { Déclarations publiées }
    property Align;
    property Font;

    property Directory : string read GetDirectory write SetDirectory;
    property Columns : TFileColumns read FFileColumns write SetFileColumns;
    property MultiSelection : Boolean read FMultiSelection write SetMultiSelection;
    property LineSelection : Boolean read FLineSelection write SetLineSelection;

    property OnFormat : TFormatContentEvent read FOnFormat write FOnFormat;
    property OnFormatFileName : TFormatFileNameEvent read FOnFormatFileName write FOnFormatFileName;
    property OnFileChange : TFileChangeEvent read FOnFileChange write FOnFileChange;
    property OnListChange : TNotifyEvent read FOnListChange write FOnListChange;
    property OnFileLocked : TFileLockedEvent read FOnFileLocked write FOnFileLocked;
    property OnAfterFileLocked : TNotifyEvent read FOnAfterFileLocked write FOnAfterFileLocked;
    property BeforeDrawFileName : TBeforeDrawFileNameEvent read FBeforeDrawFileName write FBeforeDrawFileName;
    property BeforeDrawColumnEvent : TBeforeDrawColumnEvent read FBeforeDrawColumn write FBeforeDrawColumn;

    property Filtered : Boolean read FFiltered write SetFiltered;
    property OnFilterItem : TFilterItemEvent read FOnFilterItem write FOnFilterItem;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

implementation

uses FGUtils, ShellAPI, FEditFileGridCols, Math;

var
  FIconList : TStringList;
  FSmallIcons : TImageList;

procedure GetIcon(const AFileName: string; AIcon: TIcon);
var
  FileInfo : TSHFileInfo;
  AExt : string;
  k : Integer;
  FNewIcon : TIcon;
const
  ExcludedExt = '.EXE.ICO.LNK';
begin
  AExt := AnsiUpperCase(ExtractFileExt(AFileName));
  if FIconList.Find(AExt, k) and (Pos(AExt, ExcludedExt) <= 0) then
  begin
    AIcon.Assign(TIcon(FIconList.Objects[k]));
  end
  else
  if SHGetFileInfo(PChar(AFileName), FILE_ATTRIBUTE_NORMAL, FileInfo,
      SizeOf(FileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or
      SHGFI_SMALLICON or SHGFI_TYPENAME) > 0 then
  begin
    FSmallIcons.GetIcon(FileInfo.iIcon, AIcon);
    if (Pos(AExt, ExcludedExt) <= 0) then
    begin
      FNewIcon := TIcon.Create;
      FNewIcon.Assign(AIcon);
      FIconList.AddObject(AExt, FNewIcon);
    end;
  end
  else
  begin
    AIcon.Assign(nil);
  end;
end;

procedure CreateImageList;
var
  FileInfo : TSHFileInfo;
begin
  FSmallIcons := TImageList.Create(nil);
  FSmallIcons.Handle := SHGetFileInfo('C:\',
    FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo),
    SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FSmallIcons.ShareImages := True;

  FIconList := TStringList.Create;
  FIconList.Sorted := True;
end;

{ TFileGrid }

procedure TFileGrid.CheckColCount;
begin
  ColCount := 1 + FFileColumns.Count;
  if ColCount > 1 then
    FixedCols := 1;
end;

procedure TFileGrid.CheckRowCount;
begin
  if FilteredFiles.Count = 0 then
    RowCount := 2
  else
    RowCount := 1 + FilteredFiles.Count;
end;

procedure TFileGrid.CheckRowHeight;
begin
  DefaultRowHeight := Canvas.TextHeight('tpqQTÒ') + 4;
end;

procedure TFileGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;
  CheckRowHeight;
end;

constructor TFileGrid.Create(AOwner: TComponent);
begin
  inherited;
  FList := TThreadedFileList.Create(TSystemFileInformation, True);
  FList.AutoLoad := False;

  FFilteredList := TThreadedFileList.Create(TSystemFileInformation, False);
  FFilteredList.AutoLoad := False;

  FSelectedList := TThreadedFileList.Create(TSystemFileInformation, False);
  FSelectedList.AutoLoad := False;



  FIcon := TIcon.Create;
  FFileColumns := TFileColumns.Create(Self, TFileColumnItem);
  FTitles := TStringList.Create;
  DefaultDrawing := False;
  Options := [goColSizing, goThumbTracking, goColMoving];
  GridLineWidth := 0;
  FixedCols := 1;
  FixedRows := 1;

  FFileNameColumn := TFileColumnItem.Create(nil);
  FFileNameColumn.FParent := Self;
  FFileNameColumn.Width := 150;

  CheckParams;

  FList.OnChange := ListChange;
  FFilteredList.OnChange := FilteredListChange;
  FSelectedList.OnChange := SelectedListChange;

end;

procedure TFileGrid.CreateWnd;
begin
  inherited;
  CheckColCount;
  CheckRowCount;
  CheckRowHeight;
end;

destructor TFileGrid.Destroy;
begin
  FFileNameColumn.Free;
  FOnFormat := nil;
  FOnListChange := nil;
  FOnFileChange := nil;
  FTitles.Free;
  FFileColumns.Free;
  FList.OnChange := nil;
  FFilteredList.OnChange := nil;
  FSelectedList.OnChange := nil;
  FList.Free;
  FFilteredList.Free;
  FSelectedList.Free;
  FIcon.Free;
  inherited;
end;

procedure TFileGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  ANom : string;
  RectText : TRect;
  RectFocus : TRect;
  AItem : TSystemFileInformation;
  FColParam : TFilePropertyParams;
  APPt : TCustomFileProperty;
  AAlignment : TAlignment;
  CanRetry : Boolean;

  procedure DessinerTexte(Rectangle : TRect; const Texte : string);
  begin
    case AAlignment of
    taLeftJustify : DrawText(Canvas.Handle, PChar(Texte), Length(Texte), Rectangle, DT_END_ELLIPSIS or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    taCenter : DrawText(Canvas.Handle, PChar(Texte), Length(Texte), Rectangle, DT_END_ELLIPSIS or DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    taRightJustify : DrawText(Canvas.Handle, PChar(Texte), Length(Texte), Rectangle, DT_END_ELLIPSIS or DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    end;
  end;
  procedure DessinerTriDesc(ALeft : Integer);
  begin
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(ALeft + 8, 4);
    Canvas.LineTo(ALeft + 1, 4);
    Canvas.LineTo(ALeft + 5, 11);
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.LineTo(ALeft + 8, 4);
  end;
  procedure DessinerTriAsc(ALeft : Integer);
  begin
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(ALeft + 1, 11);
    Canvas.LineTo(ALeft + 8, 11);
    Canvas.LineTo(ALeft + 5, 4);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.LineTo(ALeft + 1, 11);
  end;
begin
  Canvas.Font := Font;
  if ARow = 0 then
  begin
    DrawFrameControl(Canvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Style := bsClear;
    ARect.Left := ARect.Left + 4;
    ARect.Right := ARect.Right - 5;
    ARect.Bottom := ARect.Bottom - 1;
    if ACol = 0 then
    begin
      if FilteredFiles.SortIndex  < 0 then
      begin
        if FilteredFiles.SortDescending then
          DessinerTriDesc(ARect.Right - 11)
        else
          DessinerTriAsc(ARect.Right - 11);
        ARect.Right := ARect.Right - 14;
      end;
      FColParam := GetFilePropertyParams(FileClass, 'FILENAME');
      AAlignment := TAlignment(FColParam.Alignment);
      DessinerTexte(ARect, FColParam.Title);
    end
    else
    begin
      Dec(ACol);
      if ACol < FFileColumns.Count then
      begin
        AAlignment := FFileColumns[ACol].Alignment;
        FColParam := GetFilePropertyParams(FileClass, FFileColumns[ACol].PropertyName);
        if Assigned(FColParam) then
        begin
          if FilteredFiles.SortIndex = FColParam.Index then
          begin
            if FilteredFiles.SortDescending then
              DessinerTriDesc(ARect.Right - 11)
            else
              DessinerTriAsc(ARect.Right - 11);
            ARect.Right := ARect.Right - 14;
          end;
          DessinerTexte(ARect, FColParam.Title);
        end;
      end;
    end;
  end
  else
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Dec(ARow);
    if (ARow >= FilteredFiles.Count) then
      Exit;
    AItem := TSystemFileInformation(FilteredFiles[ARow]);
    if not AItem.ReadError then
      AItem.CheckLoaded;
    if not AItem.Loaded then
    begin
      if Assigned(FOnFileLocked) then
      begin
        CanRetry := False;
        FOnFileLocked(Self, AItem.FileName, CanRetry);
        if CanRetry then
        begin
          AItem.CheckLoaded;
          if Assigned(FOnAfterFileLocked) then
            FOnAfterFileLocked(Self);
        end;
      end;
    end;
    Canvas.Font.Color := Font.Color;
    //On affiche toujours en première colonne le nom du fichier... et ensuite les colonnes Columns.
    if ACol = 0 then
    begin
      FColParam := GetFilePropertyParams(FileClass, 'FILENAME');
      AAlignment := TAlignment(FColParam.Alignment);
      ANom := ExtractFileName(AItem.FileName);
      DoFormatFileName(AItem, ANom);
      RectText := ARect;
      RectText.Left := RectText.Left + 20;
      RectText.Right := RectText.Right - 1;
      if ([gdFocused, gdSelected] * AState <> [])
        or (FSelectedList.IndexOf(AItem) >= 0) then
      begin
        if Assigned(FBeforeDrawFileName) then
          FBeforeDrawFileName(Self, Canvas, AItem);
        RectFocus := RectText;
        DrawText(Canvas.Handle, PChar(ANom), Length(ANom), RectFocus, DT_CALCRECT or DT_END_ELLIPSIS or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
        RectFocus.Bottom := ARect.Bottom;
        RectFocus.Top := ARect.Top;
        RectFocus.Right := RectFocus.Right + 3;
        RectFocus.Left := RectFocus.Left - 2;
        if {(gdSelected in AState) or} (FSelectedList.IndexOf(AItem) >= 0) then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.FillRect(RectFocus);
        end;
        DessinerTexte(RectText, ANom);
        if Focused and (Selected = AItem) then //([gdFocused] * AState <> []) then
          DrawFocusRect(Canvas.Handle, RectFocus);
      end
      else
      begin
        if Assigned(FBeforeDrawFileName) then
          FBeforeDrawFileName(Self, Canvas, AItem);
        DessinerTexte(RectText, ANom);
      end;
      GetIcon(AItem.FileName, FIcon);
      Canvas.Draw(ARect.Left + 1, ARect.Top + 1, FIcon);
    end
    else
    if AItem.Loaded then
    begin
      //Et enfin affichage des colonnes supplémentaires...
      Dec(ACol);
      if ACol < FFileColumns.Count then
      begin
        FColParam := GetFilePropertyParams(FileClass, FFileColumns[ACol].PropertyName);
        if Assigned(FColParam) then
        begin
          AAlignment := FFileColumns[ACol].Alignment;
          APPt := AItem.Properties[FColParam.Index];
          ANom := APPt.AsString;
          DoFormat(APPt, ANom);
          ARect.Left := ARect.Left + 4;
          ARect.Right := ARect.Right - 5;
          if Assigned(FBeforeDrawColumn) then
            FBeforeDrawColumn(Self, Canvas, AItem, APPt);
          //Canvas.Font.Style := Font.Style;
          //Canvas.Font.Name := Font.Name;
          DessinerTexte(ARect, ANom);
        end;
      end;
    end
    else
    if AItem.ReadError then
    begin
      //Et enfin affichage des colonnes supplémentaires...
      Dec(ACol);
      if ACol < FFileColumns.Count then
      begin
        FColParam := GetFilePropertyParams(FileClass, FFileColumns[ACol].PropertyName);
        if Assigned(FColParam) then
        begin
          AAlignment := FFileColumns[ACol].Alignment;
          APPt := AItem.Properties[FColParam.Index];
          ANom := APPt.AsString;
          if ANom = StreamReadError then
          begin
            ANom := 'Erreur : ' + AItem.ReadErrorMessage;
            Canvas.Font.Style := [fsBold, fsItalic];
            Canvas.Font.Name := 'Arial';
          end;
          DoFormat(APPt, ANom);
          ARect.Left := ARect.Left + 4;
          ARect.Right := ARect.Right - 5;
          if Assigned(FBeforeDrawColumn) then
            FBeforeDrawColumn(Self, Canvas, AItem, APPt);
          DessinerTexte(ARect, ANom);
        end;
      end;
    end;
  end;
end;

function TFileGrid.GetFileClass: TFileClass;
begin
  Result := FList.FileClass;
end;

procedure TFileGrid.CheckParams;
begin
  //C'est une façon de s'assurer que les PropertyParams existent tous...
  with FileClass.Create('', False) do
  begin
    FPropertyCount := PropertyCount;
    Free;
  end;
end;

procedure TFileGrid.ListChange(Sender: TObject);
begin
  if Filtered then
    Filter
  else
  begin
    CheckRowCount;
    Invalidate;
    if Assigned(FOnListChange) then
      FOnListChange(Self);
  end;
end;

procedure TFileGrid.Loaded;
begin
  inherited;
  if Filtered then
    Filter;
  CheckRowHeight;
  CheckColCount;
  CheckRowCount;
end;

procedure TFileGrid.Paint;
begin
  FNeedInvalidate := False;
  inherited;
  if FNeedInvalidate then
    Invalidate;
end;

procedure TFileGrid.SetDirectory(const Value: string);
  procedure CheckLimits;
  begin
    FTrueFileCount := FList.Count;
    if (FileCountLimit > 0) and (FList.Count > FileCountLimit) then
    begin
      FList.BeginUpdate;
      try
        while FList.Count > FileCountLimit do
          FList.Delete(FList.Count - 1);
      finally
        FList.EndUpdate;
      end;
    end;
  end;
begin
  if (FList.Directory <> Value) then
  begin
    FSelStart := 0;
    FSelectedList.Clear;
    FList.Clear;
    FFilteredList.Clear;
    Col := 0;
    Row := 1;
    FSelStart := 0;
    FSelectedList.Clear;
    FList.Directory := Value;
    FList.SortIndex := SortIndex;
    FList.SortDescending := SortDescending;
    FList.Sort;
    CheckLimits;
    CheckRowCount;
    if Filtered then
      Filter;
    Refresh;
    FList.BackgroundFilling;
  end;
end;

procedure TFileGrid.SetFileClass(const Value: TFileClass);
begin
  FSelStart := 0;
  FSelectedList.Clear;
  FSelectedList.FileClass := Value;
  FList.Clear;
  FList.FileClass := Value;
  FFilteredList.Clear;
  FFilteredList.FileClass := Value;
  CheckParams;
  FList.Fill;
  if Filtered then
    Filter;
end;

procedure TFileGrid.SetFileColumns(const Value: TFileColumns);
begin
  FFileColumns.Assign(Value);
end;

procedure TFileGrid.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_PROPERTYPARAMCHANGED then
    Invalidate;
end;

function TFileGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  ANew : TCustomFileInformation;
begin
  Result := (ACol = 0) or (FLineSelection);
  if Result then
  begin
    if Assigned(FOnFileChange) then
    begin
      if ((Row > 0) and (Row - 1 < FilteredFiles.Count)) then
        ANew := FilteredFiles.Items[ARow - 1]
      else
        ANew := nil;
      FOnFileChange(Self, ANew, Result);
    end;
  end;
  if Result then
  begin
    if ((Row > 0) and (Row - 1 < FilteredFiles.Count)) then
      ANew := FilteredFiles.Items[ARow - 1]
    else
      ANew := nil;
    if MultiSelection then
    begin
      if (FSelStart < 0) and Assigned(ANew) then
      begin
        if FSelectedList.IndexOf(ANew) < 0 then
        begin
          FSelStart := ARow;
          FIgnoreSelClick := True;
          FSelectedList.Add(ANew);
          InvalidateCell(0, ARow);
        end;
      end
      {else
      begin
        FSelectedList.Clear;
        if Assigned(ANew) then
          FSelectedList.Add(ANew);
      end;}
    end
    else
    begin
      FSelectedList.Clear;
      if Assigned(ANew) then
        FSelectedList.Add(ANew);
    end;
  end;
end;

function TFileGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TFileGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Perform(WM_VSCROLL, SB_LINEDOWN, 0);
  Result := True;
end;

function TFileGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Perform(WM_VSCROLL, SB_LINEUP, 0);
  Result := True;
end;

procedure TFileGrid.DoFormat(AProperty: TCustomFileProperty; var AValue: string);
begin
  if Assigned(FOnFormat) then
    FOnFormat(Self, AProperty, AValue);
end;

function TFileGrid.GetFileName: string;
begin
  if ((Row > 0) and (Row - 1 < FilteredFiles.Count)) then
    Result := FilteredFiles.Items[Row - 1].FileName
  else
    Result := '';
end;

function TFileGrid.GetSelected: TCustomFileInformation;
begin
  if ((Row > 0) and (Row - 1 < FilteredFiles.Count)) then
    Result := FilteredFiles.Items[Row - 1]
  else
    Result := nil;
end;

procedure TFileGrid.InvalidateColonneFilename;
var
  c : Integer;
begin
  for c := TopRow to TopRow + VisibleRowCount do
  begin
    InvalidateCell(0, c);
  end;
end;

procedure TFileGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow : Integer;
  i : Integer;
begin
  if Button in [mbLeft, mbRight] then
  begin
    FPotentielSortPt := Point(X, Y);
    MouseToCell(X, Y, ACol, ARow);
    if (ARow = 0) and (Button = mbLeft) then
      FPotentialSortCol := ACol
    else
    if (ACol = 0) or (FLineSelection) then
    begin
      if (ssShift in Shift) and (Button = mbLeft) then
      begin
        //Sélection d'une plage de fichiers
        if (Row > 0) and (Row <= FilteredFiles.Count) and (ARow > 0) and (ARow <= FilteredFiles.Count)then
        begin
          FSelectedList.BeginUpdate;
          try
            if MultiSelection then
            begin
              if FSelStart = -1 then
                FSelStart := Row;
              FSelectedList.Clear;
              for i := Min(FSelStart, ARow) to Max(FSelStart, ARow) do
              begin
                if FSelectedList.IndexOf(FilteredFiles.Items[i - 1]) < 0 then
                  FSelectedList.Add(FilteredFiles.Items[i - 1]);
              end;
              if Button = mbRight then
                Row := ARow;
              InvalidateColonneFilename;
            end;
            {else
            begin
              FSelectedList.Clear;
              if Assigned(Selected) then
                FSelectedList.Add(Selected);
            end;}
          finally
            FSelectedList.EndUpdate;
          end;
        end;
      end
      else
      if ([ssShift, ssCtrl] * Shift = []) and (not FIgnoreSelClick) then
      begin
        if Button = mbRight then
        begin
          if ARow >= 1 then
          begin
            Row := ARow;
            FSelStart := ARow;
            FSelectedList.Clear;
            if Assigned(Selected) then
              FSelectedList.Add(Selected);
          end;
        end
        else
        begin
          FSelStart := -1;
          FSelectedList.Clear;
        end;
        InvalidateColonneFilename;
      end;
    end;
  end;
  inherited;
end;

procedure TFileGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow : Integer;
  FColParam : TFilePropertyParams;
begin
  inherited;
  if (FPotentialSortCol >= 0) and (Button = mbLeft) then
  begin
    if (Abs(FPotentielSortPt.X - X) > 1) or (Abs(FPotentielSortPt.Y - Y) > 1) then
      Exit;
    //Pt := ScreenToClient(Mouse.CursorPos);
    //MouseToCell(X, Y, ACol, ARow);
    ACol := FPotentialSortCol;
    if ACol = 0 then //FileName
      FilteredFiles.SortIndex := -1
    else //Columns
    begin
      FColParam := GetFilePropertyParams(FileClass, Columns[ACol - 1].PropertyName);
      if Assigned(FColParam) then
        FilteredFiles.SortIndex := FColParam.Index
      else
        Exit;
    end;
    FilteredFiles.Sort;
    Invalidate;
  end
  else
  if (Button = mbLeft) and (ssCtrl in Shift) and (not (ssShift in Shift))  then
  begin
    MouseToCell(X, Y, ACol, ARow);
    if ((ACol = 0) or (FLineSelection)) and (ARow > 0) and (not FIgnoreSelClick) and (ARow <= FilteredFiles.Count) then
    begin
      if MultiSelection then
      begin
        if FSelStart < 0 then
        begin
          FSelStart := ARow;
          if FSelectedList.IndexOf(FilteredFiles.Items[ARow - 1]) < 0 then
          begin
            FSelectedList.Add(FilteredFiles.Items[ARow - 1]);
            InvalidateCell(0, ARow);
          end;
        end
        else
        begin
          FSelStart := ARow;
          if FSelectedList.IndexOf(FilteredFiles.Items[ARow - 1]) < 0 then
            FSelectedList.Add(FilteredFiles.Items[ARow - 1])
          else
            FSelectedList.Remove(FilteredFiles.Items[ARow - 1]);
          InvalidateCol(0);
        end;
      end;
    end;
  end;
  FIgnoreSelClick := False;
end;

procedure TFileGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FPotentialSortCol := -1;
end;

procedure TFileGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

function TFileGrid.GetDirectory: string;
begin
  Result := FList.Directory;
end;

procedure TFileGrid.Filter(AFilterEvent: TFilterItemEvent = nil);
var
  AEvent : TFilterItemEvent;
  AcceptFile : Boolean;
  AItem : TCustomFileInformation;
  i : Integer;
begin
  if csLoading in ComponentState then
    Exit;
  FFilteredList.BeginUpdate;
  try
    if Assigned(AFilterEvent) then
      AEvent := AFilterEvent
    else
      AEvent := FOnFilterItem;

    if Assigned(AEvent) then
    begin
      FFilteredList.Clear;
      for i := 0 to FList.Count - 1 do
      begin
        AItem := FList[i];
        AcceptFile := True;
        AEvent(Self, AItem, AcceptFile);
        if AcceptFile then
          FFilteredList.Add(AItem);
      end;
    end
    else
      FFilteredList.Assign(FList);
    Sort;
  finally
    FFilteredList.EndUpdate;
  end;
end;

procedure TFileGrid.SetFiltered(const Value: Boolean);
begin
  if FFiltered <> Value then
  begin
    //FFilteredList.SortIndex := FList.SortIndex;
    //FFilteredList.SortDescending := FList.SortDescending;
    FFiltered := Value;
    if Value then
      Filter
    else
      ListChange(Self);
    Refresh;
  end;
end;

function TFileGrid.GetFilteredList: TThreadedFileList;
begin
  if Filtered then
    Result := FFilteredList
  else
    Result := FList;
end;

procedure TFileGrid.FilteredListChange(Sender: TObject);
begin
  if not Filtered then
    Exit;
  CheckRowCount;
  Invalidate;
  if Assigned(FOnListChange) then
    FOnListChange(Self);
end;

procedure TFileGrid.SelectedListChange(Sender: TObject);
begin
  //InvalidateCol(0);
end;

procedure TFileGrid.ColumnMoved(FromIndex, ToIndex: Integer);
var
  Item1 : TFileColumnItem;
begin
  if (FromIndex > 0) and (ToIndex > 0) then //On ignore la colonne FileName
  begin
    Item1 := Columns[FromIndex - 1];
    Item1.Index := ToIndex - 1;
    inherited;
  end;
end;

function TFileGrid.BeginColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := (Origin > 0) and (Destination > 0);
end;

function TFileGrid.EndColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := (Origin > 0) and (Destination > 0);
end;

function TFileGrid.CheckColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := (Origin > 0) and (Destination > 0);
end;

procedure TFileGrid.SetMultiSelection(const Value: Boolean);
begin
  FMultiSelection := Value;
  FSelectedList.Clear;
  if Assigned(Selected) then
    FSelectedList.Add(Selected);
end;

procedure TFileGrid.SetSelected(const Value: TCustomFileInformation);
begin
  SelectedIndex := FilteredFiles.IndexOf(Value);  
end;

function TFileGrid.GetSelectedIndex: Integer;
begin
  if (Row > 0) and (Row - 1 < FilteredFiles.Count) then
    Result := Row - 1
  else
    Result := -1;
end;

procedure TFileGrid.SetSelectedIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FilteredFiles.Count) then
  begin
    if Row = Value + 1 then
      SelectCell(Col, Row)
    else
      Row := Value + 1
  end
  else
  begin
    if Row = 1 then
      SelectCell(Col, Row)
    else
      Row := 1;
  end;
  FSelStart := Row;
  FSelectedList.Clear;
  if Assigned(Selected) then
    FSelectedList.Add(Selected);
  InvalidateColonneFilename;
end;


procedure TFileGrid.DblClick;
begin
  inherited;
  FSelStart := Row;
  FSelectedList.Clear;
  if Assigned(Selected) then
    FSelectedList.Add(Selected);
  InvalidateColonneFilename;
end;

procedure TFileGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  i : Integer;
begin
  inherited;
  if (Key = VK_DOWN) or (Key = VK_UP)
  or (Key = VK_PRIOR) or (Key = VK_NEXT)
  or (Key = VK_END) or (Key = VK_HOME) then
  begin
    if not (ssShift in Shift) then
    begin
      FSelStart := Row;
      FSelectedList.Clear;
      if Assigned(Selected) then
        FSelectedList.Add(Selected);
      InvalidateColonneFilename;
    end
    else
    begin
      FSelectedList.BeginUpdate;
      try
        if MultiSelection then
        begin
          if FSelStart = -1 then
            FSelStart := Row;
          FSelectedList.Clear;
          for i := Min(FSelStart, Row) to Max(FSelStart, Row) do
          begin
            if FSelectedList.IndexOf(FilteredFiles.Items[i - 1]) < 0 then
              FSelectedList.Add(FilteredFiles.Items[i - 1]);
          end;
          InvalidateColonneFilename;
        end
        else
        begin
          FSelectedList.Clear;
          if Assigned(Selected) then
            FSelectedList.Add(Selected);
          InvalidateColonneFilename;
        end;
      finally
        FSelectedList.EndUpdate;
      end;
    end;
  end;
end;

procedure TFileGrid.SetFileName(const Value: string);
begin
  Selected := FList.FindByName(Value);
end;

function TFileGrid.GetSortDescending: Boolean;
begin
  Result := FFilteredList.SortDescending;
end;

function TFileGrid.GetSortIndex: Integer;
begin
  Result := FFilteredList.SortIndex;
end;

procedure TFileGrid.SetSortDescending(const Value: Boolean);
begin
  FFilteredList.SortDescending := Value;
  FList.SortDescending := Value;
end;

procedure TFileGrid.SetSortIndex(const Value: Integer);
begin
  FFilteredList.SortIndex := Value;
  FList.SortIndex := Value;
end;

procedure TFileGrid.Sort;
begin
  FFilteredList.Sort;
  Invalidate;
end;

procedure TFileGrid.SetLineSelection(const Value: Boolean);
begin
  FLineSelection := Value;
end;

procedure TFileGrid.DoFormatFileName(AItem : TCustomFileInformation; var AValue: string);
begin
  if Assigned(FOnFormatFileName) then
    FOnFormatFileName(Self, AItem, AValue);
end;

procedure TFileGrid.InvalidateFile(AFileName: string);
var
  AIdx : Integer;
begin
  AIdx := Files.IndexOf(AFileName);
  if AIdx >= 0 then
    InvalidateRow(AIdx + 1);
end;

procedure TFileGrid.InvalidateFile(AItem: TCustomFileInformation);
var
  AIdx : Integer;
begin
  if Assigned(AItem) then
  begin
    AIdx := Files.IndexOf(AItem);
    if AIdx >= 0 then
      InvalidateRow(AIdx + 1);
  end;
end;

{ TFileColumns }

function TFileColumns.Add: TFileColumnItem;
begin
  Result := TFileColumnItem(inherited Add);
end;

constructor TFileColumns.Create(AParent: TFileGrid; AItemClass: TFileColumnItemClass);
begin
  inherited Create(AItemClass);
  FParent := AParent;
end;

procedure TFileColumns.Edit(CheckTitle : Boolean = False);
var
  AFen : TDlgEditFileGridColumns;
begin
  AFen := TDlgEditFileGridColumns.Create(nil);
  try
    AFen.Init(Parent, CheckTitle);
    AFen.ShowModal;
  finally
    AFen.Free;
  end;
end;

procedure TFileColumns.CheckColumnsParams;
var
  i : Integer;
  AColParam : TFilePropertyParams;
  AColumn : TFileColumnItem;
begin
  BeginUpdate;
  try
    for i := 0 to Count - 1 do
    begin
      AColumn := Items[i];
      AColParam := GetFilePropertyParams(Parent.FileClass, AColumn.PropertyName);
      AColumn.DisplayName := AColParam.Title;
      AColumn.Width := AColParam.ColWidth;
      AColumn.Alignment := AColParam.Alignment;
    end;
  finally
    EndUpdate;
  end;
end;

function TFileColumns.GetColumnItem(Index: Integer): TFileColumnItem;
begin
  Result:= TFileColumnItem(inherited Items[Index]);
end;

function TFileColumns.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TFileColumns.SetColumnItem(Index: Integer;
  const Value: TFileColumnItem);
begin
  Items[Index].Assign(Value);
end;

procedure TFileColumns.Update(Item: TCollectionItem);
begin
  inherited;
  FParent.CheckColCount;
  FParent.Invalidate;
end;

{ TFileColumnItem }

constructor TFileColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FAlignment := taLeftJustify;
end;

destructor TFileColumnItem.Destroy;
begin
  inherited;
end;

function TFileColumnItem.GetDisplayName: string;
begin
  Result := FPropertyName;
end;

function TFileColumnItem.GetParent: TFileGrid;
begin
  if Assigned(FParent) then
    Result := FParent
  else
    Result := TFileColumns(Collection).Parent;
end;

function TFileColumnItem.GetWidth: Integer;
begin
  if (Index + 1 < Parent.ColCount) then
    Result := Parent.ColWidths[Index + 1]
  else
    Result := -1;
end;

procedure TFileColumnItem.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Parent.Invalidate;
end;

procedure TFileColumnItem.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
  Parent.Invalidate;
end;

procedure TFileColumnItem.SetWidth(const Value: Integer);
begin
  if (Index + 1 < Parent.ColCount) then
    Parent.ColWidths[Index + 1] := Value;
end;

initialization
  CreateImageList;
finalization
  FSmallIcons.Free;
  FSmallIcons := nil;
  ClearStringList(FIconList);
  FIconList.Free;
  FIconList := nil;
end.
