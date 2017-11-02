unit FToolbarWysiwyg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ToolWin, ImgList, ProfDHTMLEdit;

type
  TToolBarWysiwyg = class(TFrame)
    ToolBar1: TToolBar;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonPrint: TToolButton;
    ToolButtonPreview: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonCut: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonSeparator3: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolButtonRedo: TToolButton;
    ToolBar2: TToolBar;
    ComboBoxFont: TComboBox;
    ToolButtonSeparator4: TToolButton;
    ComboBoxFontSize: TComboBox;
    ToolButtonSeparator5: TToolButton;
    ToolButtonBold: TToolButton;
    ToolButtonItalic: TToolButton;
    ToolButtonUnderline: TToolButton;
    ToolButtonSeparator6: TToolButton;
    ToolButtonNumbering: TToolButton;
    ToolButtonBullets: TToolButton;
    ToolButtonDecInd: TToolButton;
    ToolButtonIncInd: TToolButton;
    ToolButtonSeparator7: TToolButton;
    ToolButtonLeft: TToolButton;
    ToolButtonCenter: TToolButton;
    ToolButtonRight: TToolButton;
    ToolButtonJustify: TToolButton;
    ToolButtonSeparator8: TToolButton;
    ToolButtonInsertHorizontalLine: TToolButton;
    ToolButtonInsertHyperlink: TToolButton;
    ToolButtonInsertPicture: TToolButton;
    ToolButtonSeparator9: TToolButton;
    ToolButtonBackColor: TToolButton;
    ToolButtonForeColor: TToolButton;
    ImageListToolBar2: TImageList;
    ImageListToolBar1: TImageList;
    ColorDialog1: TColorDialog;
    ToolButtonPrintSetup: TToolButton;
    ToolButtonSeparator11: TToolButton;
    ToolButtonFind: TToolButton;
    ToolButton1: TToolButton;
    ComboBoxStyle: TComboBox;
    ToolButtonAlignNone: TToolButton;
    procedure ToolButtonBackColorClick(Sender: TObject);
    procedure ToolButtonBoldClick(Sender: TObject);
    procedure ToolButtonBulletsClick(Sender: TObject);
    procedure ToolButtonCenterClick(Sender: TObject);
    procedure ToolButtonCopyClick(Sender: TObject);
    procedure ToolButtonCutClick(Sender: TObject);
    procedure ToolButtonDecIndClick(Sender: TObject);
    procedure ToolButtonForeColorClick(Sender: TObject);
    procedure ToolButtonIncIndClick(Sender: TObject);
    procedure ToolButtonInsertHorizontalLineClick(Sender: TObject);
    procedure ToolButtonInsertHyperlinkClick(Sender: TObject);
    procedure ToolButtonInsertPictureClick(Sender: TObject);
    procedure ToolButtonItalicClick(Sender: TObject);
    procedure ToolButtonJustifyClick(Sender: TObject);
    procedure ToolButtonLeftClick(Sender: TObject);
    procedure ToolButtonNewClick(Sender: TObject);
    procedure ToolButtonNumberingClick(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonPasteClick(Sender: TObject);
    procedure ToolButtonPreviewClick(Sender: TObject);
    procedure ToolButtonPrintClick(Sender: TObject);
    procedure ToolButtonRedoClick(Sender: TObject);
    procedure ToolButtonRightClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure ToolButtonUnderlineClick(Sender: TObject);
    procedure ToolButtonUndoClick(Sender: TObject);
    procedure ComboBoxFontChange(Sender: TObject);
    procedure ComboBoxFontSizeChange(Sender: TObject);
    procedure ToolButtonPrintSetupClick(Sender: TObject);
    procedure ToolButtonAlignNoneClick(Sender: TObject);
    procedure ToolButtonFindClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure FillStyles;
  public
    { Déclarations publiques }
    Editor : TProfDHTMLEdit;
    procedure Init(AEditor : TProfDHTMLEdit);
    procedure DoDisplayChanged(Sender : TObject);
  end;

implementation

{$R *.DFM}


function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Strings: TStrings): Integer; stdcall;
begin
  Strings.Add(LogFont.lfFaceName);
  Result := 1
end;

procedure TToolBarWysiwyg.ToolButtonBoldClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.ToggleBold;
end;

procedure TToolBarWysiwyg.ToolButtonItalicClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.ToggleItalic
end;

procedure TToolBarWysiwyg.ToolButtonUnderlineClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.ToggleUnderline
end;

procedure TToolBarWysiwyg.ToolButtonUndoClick(Sender: TObject);
begin
  Editor.Undo
end;

procedure TToolBarWysiwyg.ToolButtonRedoClick(Sender: TObject);
begin
  Editor.Redo
end;

procedure TToolBarWysiwyg.ToolButtonCutClick(Sender: TObject);
begin
  Editor.CutToClipboard
end;

procedure TToolBarWysiwyg.ToolButtonCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard
end;

procedure TToolBarWysiwyg.ToolButtonPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard
end;

procedure TToolBarWysiwyg.ToolButtonNewClick(Sender: TObject);
begin
  if MessageDlg('Confirmez-vous la suppression du texte en cours de saisie ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    Editor.NewDocument;
  end;
end;

procedure TToolBarWysiwyg.ToolButtonPrintClick(Sender: TObject);
begin
  Editor.Print
end;

procedure TToolBarWysiwyg.ToolButtonPreviewClick(Sender: TObject);
begin
  Editor.PrintPreviewDialog
end;

procedure TToolBarWysiwyg.ToolButtonNumberingClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.ToggleNumbering
end;

procedure TToolBarWysiwyg.ToolButtonBulletsClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.ToggleBullets
end;

procedure TToolBarWysiwyg.ToolButtonDecIndClick(Sender: TObject);
begin
  Editor.DecreaseIndentation
end;

procedure TToolBarWysiwyg.ToolButtonIncIndClick(Sender: TObject);
begin
  Editor.IncreaseIndentation
end;

procedure TToolBarWysiwyg.ToolButtonLeftClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.Justify := JustifyLeft
end;

procedure TToolBarWysiwyg.ToolButtonCenterClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.Justify := JustifyCenter
end;

procedure TToolBarWysiwyg.ToolButtonRightClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.Justify := JustifyRight
end;

procedure TToolBarWysiwyg.ToolButtonJustifyClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.Justify := JustifyFull
end;

procedure TToolBarWysiwyg.ToolButtonInsertHyperlinkClick(Sender: TObject);
begin
  Editor.HyperlinkDialog;
end;

procedure TToolBarWysiwyg.ToolButtonInsertPictureClick(Sender: TObject);
begin
  Editor.PictureDialog
end;

procedure TToolBarWysiwyg.ToolButtonForeColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    Editor.ForeColor := ColorDialog1.Color
end;

procedure TToolBarWysiwyg.ToolButtonBackColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    Editor.BackColor := ColorDialog1.Color
end;

procedure TToolBarWysiwyg.ToolButtonSaveClick(Sender: TObject);
begin
  Editor.SaveDialog('');
end;

procedure TToolBarWysiwyg.ToolButtonOpenClick(Sender: TObject);
begin
  Editor.OpenDialog('');
end;

procedure TToolBarWysiwyg.ToolButtonInsertHorizontalLineClick(Sender: TObject);
begin
  Editor.InsertHorizontalLine;
end;

procedure TToolBarWysiwyg.ComboBoxFontChange(Sender: TObject);
begin
  Editor.FontName := (Sender as TComboBox).Text
end;

procedure TToolBarWysiwyg.ComboBoxFontSizeChange(Sender: TObject);
begin
  Editor.FontHTMLSize := FontSizeToHTMLFontSize(StrToInt((Sender as TComboBox).Text))
end;

procedure TToolBarWysiwyg.ToolButtonPrintSetupClick(Sender: TObject);
begin
  Editor.PageSetupDialog;
end;

procedure TToolBarWysiwyg.Init(AEditor: TProfDHTMLEdit);
var
  DC: HDC;
begin
  Editor := AEditor;

  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(ComboBoxFont.Items));
  ReleaseDC(0, DC);
  ComboBoxFont.Sorted := True;

  FillStyles;

  DoDisplayChanged(Editor);
end;

procedure TToolBarWysiwyg.DoDisplayChanged(Sender : TObject);
var
  Justify: TJustify;
  {TextSize: TTextSize;
  TableRows, TableCols: Integer;
  TableCaption: WideString;}
begin
  if not Assigned(Editor) then
    Exit;

  ToolButtonNew.Enabled := True;
  ToolButtonOpen.Enabled := Editor.CanLoadFromFile;
  ToolButtonSave.Enabled := Editor.CanSave;
  ToolButtonPrint.Enabled := Editor.CanPrint;
  ToolButtonPreview.Enabled := Editor.CanPrint;
  ToolButtonPrintSetup.Enabled := Editor.CanPrint;

  ToolButtonCut.Enabled := Editor.CanCut;
  ToolButtonCopy.Enabled := Editor.CanCopy;
  ToolButtonPaste.Enabled := Editor.CanPaste;
  ToolButtonUndo.Enabled := Editor.CanUndo;
  ToolButtonRedo.Enabled := Editor.CanRedo;
  ToolButtonFind.Enabled := Editor.CanFind;

  ComboBoxFont.ItemIndex := ComboBoxFont.Items.IndexOf(Editor.Font.Name);
  ComboBoxFont.Enabled := Editor.CanSetFontName;
  ComboBoxFontSize.ItemIndex := ComboBoxFontSize.Items.IndexOf(IntToStr(Editor.Font.Size));
  ComboBoxFontSize.Enabled := Editor.CanSetFontSize;

  ToolButtonBold.Down := Editor.Bold;
  ToolButtonBold.Enabled := Editor.CanToggleBold;
  ToolButtonItalic.Down := Editor.Italic;
  ToolButtonitalic.Enabled := Editor.CanToggleItalic;
  ToolButtonUnderline.Down := Editor.Underline;
  ToolButtonUnderline.Enabled := Editor.CanToggleUnderline;

  ToolButtonNumbering.Down := Editor.Numbering;
  ToolButtonNumbering.Enabled := Editor.CanToggleNumbering;
  ToolButtonBullets.Down := Editor.Bullets;
  ToolButtonBullets.Enabled := Editor.CanToggleBullets;

  ToolButtonDecInd.Enabled := Editor.CanIncreaseIndentation;
  ToolButtonIncInd.Enabled := Editor.CanDecreaseIndentation;

  Justify := Editor.Justify;
  ToolButtonLeft.Down := Justify = JustifyLeft;
  ToolButtonLeft.Enabled := Editor.CanJustifyLeft;
  ToolButtonCenter.Down := Justify = JustifyCenter;
  ToolButtonCenter.Enabled := Editor.CanJustifyCenter;
  ToolButtonRight.Down := Justify = JustifyRight;
  ToolButtonRight.Enabled := Editor.CanJustifyRight;
  ToolButtonJustify.Down := Justify = JustifyFull;
  ToolButtonJustify.Enabled := Editor.CanJustifyFull;
  ToolButtonAlignNone.Down := Justify = JustifyNone;
  ToolButtonAlignNone.Enabled := Editor.CanJustifyNone;

  ToolButtonInsertHorizontalLine.Enabled := Editor.CanInsertHorizontalLine;
  ToolButtonInsertHyperlink.Enabled := Editor.CanDisplayHyperlinkDialog;
  ToolButtonInsertPicture.Enabled := Editor.CanInsertImage;
  ToolButtonBackColor.Enabled := Editor.CanSetBackColor;
  ToolButtonForeColor.Enabled := Editor.CanSetForeColor;

  {MenuItemInsertTextBox.Enabled := Editor.CanInsertTextBox;
  MenuItemInsertTextArea.Enabled := Editor.CanInsertTextArea;
  MenuItemInsertCheckBox.Enabled := Editor.CanInsertCheckBox;
  MenuItemInsertRadioButton.Enabled := Editor.CanInsertRadioButton;
  MenuItemInsertDropdownBox.Enabled := Editor.CanInsertDropdownBox;
  MenuItemInsertListBox.Enabled := Editor.CanInsertListBox;
  MenuItemInsertButton.Enabled := Editor.CanInsertButton;}

  {MenuItemTableInsertTable.Enabled := Editor.CanInsertTable;
  MenuItemTableInsertColumn.Enabled := Editor.CanTableInsertColumn;
  MenuItemTableInsertRow.Enabled := Editor.CanTableInsertRow;
  MenuItemTableInsertCell.Enabled := Editor.CanTableInsertCell;
  MenuItemTableDeleteColumn.Enabled := Editor.CanTableDeleteColumn;
  MenuItemTableDeleteRow.Enabled := Editor.CanTableDeleteRow;
  MenuItemTableDeleteCell.Enabled := Editor.CanTableDeleteCell;
  MenuItemTableMergeCells.Enabled := Editor.CanTableMergeCells;
  MenuItemTableSplitCell.Enabled := Editor.CanTableSplitCell;
  MenuItemTableProperties.Enabled := Editor.IsTableSelected;
  MenuItemTableShowBorders.Checked := Editor.ShowBorders;}

  FillStyles;
  
end;

procedure TToolBarWysiwyg.FillStyles;
begin
  if not Assigned(Editor) then
    Exit;
  ComboBoxStyle.Enabled := Editor.CanSetParagraphStyle;
  if ComboBoxStyle.Enabled then
  begin
    if Editor.GetParagraphStyles(ComboBoxStyle.Items) then
      ComboBoxStyle.ItemIndex := ComboBoxStyle.Items.IndexOf(Editor.ParagraphStyle);
  end;
end;

procedure TToolBarWysiwyg.ToolButtonAlignNoneClick(Sender: TObject);
begin
  (Sender as TToolButton).Down := True;
  Editor.Justify := JustifyNone;
end;

procedure TToolBarWysiwyg.ToolButtonFindClick(Sender: TObject);
begin
  Editor.FindDialog;
end;

end.
