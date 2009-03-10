unit FGCPGlyf;

interface

{$I GrizzlyDefine.INC}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  forms, Dialogs, Buttons, StdCtrls, FileCtrl, IniFiles, ExtCtrls
  {$IFDEF GZ_D6}
  ,DesignIntf, DesignEditors
  {$ELSE}
  ,DsgnIntf
  {$ENDIF};

type
  TDlgGlyph = class(Tform)
    BtnFinal: TBitBtn;
    BtnCancel: TBitBtn;
    BtnNext: TBitBtn;
    BtnPrior: TBitBtn;
    DirListBox: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    CBEnglish: TCheckBox;
    PanelAbout: TPanel;
    Memo1: TMemo;
    Panel1: TPanel;
    BtnCloseAbout: TBitBtn;
    BtnAbout: TBitBtn;
    RGButtons: TRadioGroup;
    LabelFiltrer: TLabel;
    EditFiltrer: TEdit;
    procedure formCreate(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPriorClick(Sender: TObject);
    procedure formDestroy(Sender: TObject);
		procedure BtnGlyphClick(Sender : TObject);
    procedure DirListBoxChange(Sender: TObject);
    procedure CBEnglishClick(Sender: TObject);
    procedure BtnCloseAboutClick(Sender: TObject);
    procedure RGButtonsClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
    procedure EditFiltrerChange(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private-déclarations }
  public
    { Public-déclarations }
    ActiveIndex : Longint;
    BtnList : TList;
    BmpList : TStringList;
    IgnoreClick : Boolean;
    ActiveDirectory,FirstDirectory : string;
    HeightCount, WidthCount : Integer;
    procedure FreeBtnList;
    procedure CreateBtnList;
    procedure FillButtons;
		procedure FillBmpList;
  end;

	TGlyphProperty = class(TPropertyEditor)
  public
  	function GetValue : string; override;
    procedure SetValue(const Value : string) ; override;
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

procedure Register;

implementation

{$R *.DFM}

{$IFDEF GZ_D4}
uses Menus;
{$ENDIF}


{ TGlyphProperty }


function TGlyphProperty.GetValue : string;
begin
	{Si l'objet est une image et que le handle GDI est valide, alors...}
	if (TBitmap(GetOrdValue) is TBitmap) and (TBitmap(GetOrdValue).Handle<>0) then
		Result:='(TBitmap)'
  else
  begin
  	Result:='(vide/empty)';
  end;
end;

procedure TGlyphProperty.SetValue(const Value : string);
begin
	{On inhibe la saisie au clavier}
end;

procedure TGlyphProperty.Edit;
var
	ADlg : TDlgGlyph;
begin
	ADlg:=TDlgGlyph.Create(Application);
  try
		if ADlg.ShowModal=mrOk then
    begin
    	SetOrdValue(Longint(ADlg.BtnFinal.Glyph));
    end;
  finally
  	ADlg.Free;
  end;
end;

function TGlyphProperty.GetAttributes : TPropertyAttributes;
begin
	Result:=[paDialog];
end;


{ TDlgGlyph }

procedure TDlgGlyph.formCreate(Sender: TObject);
var
  AIni : TIniFile;
begin
  BmpList:=TStringList.Create;
  BmpList.Sorted:=True;

	BtnList:=TList.Create;

  HeightCount:=10;
  WidthCount:=4;

	FirstDirectory:=DirListBox.Directory;

	IgnoreClick:=True;

	AIni:=TIniFile.Create('DELPHI.INI');
  try
    ActiveDirectory:=AIni.ReadString('GUILLIEN, Editeur de Glyph','Dernier chemin','C:\');
    CBEnglish.Checked:=AIni.ReadInteger('GUILLIEN, Editeur de Glyph','Langue',1)=1;
    RGButtons.ItemIndex:=AIni.ReadInteger('GUILLIEN, Editeur de Glyph','Boutons',1);
  finally
    AIni.Free;
  end;

  DirListBox.Directory:=ActiveDirectory;

  IgnoreClick:=False;

  if ActiveDirectory[Length(ActiveDirectory)]<>'\' then
  	ActiveDirectory:=ActiveDirectory+'\';

  FillBmpList;
  CreateBtnList;
end;

procedure TDlgGlyph.FormDestroy(Sender: TObject);
var
	AIni : TIniFile;
begin
	AIni:=TIniFile.Create('DELPHI.INI');
  try
    AIni.WriteString('GUILLIEN, Editeur de Glyph','Dernier chemin',DirListBox.Directory);
    if CBEnglish.Checked then
      AIni.WriteInteger('GUILLIEN, Editeur de Glyph','Langue',1)
    else
      AIni.WriteInteger('GUILLIEN, Editeur de Glyph','Langue',0);
    AIni.WriteInteger('GUILLIEN, Editeur de Glyph','Boutons',RGButtons.ItemIndex);
  finally
    AIni.Free;
  end;

  IgnoreClick:=True;

  {Necessary to restore the working dir in Delphi}
  DirListBox.Directory:=FirstDirectory;

	BtnList.Free;
  BmpList.Free;
end;

procedure TDlgGlyph.FreeBtnList;
var
  i : Longint;
begin
  if BtnList.Count>0 then
    for i:=0 to BtnList.Count - 1 do
    begin
      TObject(BtnList.Items[i]).Free;
      BtnList.Items[i]:=Nil;
    end;
  BtnList.Clear;
end;

procedure TDlgGlyph.CreateBtnList;
var
  i : Longint;
  WDispo, HDispo : Integer;
  LBtn, HBtn : Integer;
  function CreateButton : TBitBtn;
  begin
	  Result:=TBitBtn.Create(Self);
  	Result.Parent:=Self;
	  Result.Width:=LBtn;
	  Result.Height:=HBtn;
	  Result.Left:=4+(i mod WidthCount)*LBtn;
	  Result.Top:=(i div WidthCount)*HBtn+4;
    Result.Margin:=4;
    Result.Spacing:=4;
		Result.OnClick:=BtnGlyphClick;
  end;
begin
  FreeBtnList;
  WDispo:=ClientWidth-12-DirListBox.Width;
  HDispo:=BtnFinal.Top-6;
  LBtn:=WDispo div WidthCount;
  HBtn:=HDispo div HeightCount;
  for i:=0 to WidthCount * HeightCount - 1 do
  begin
    BtnList.Add(CreateButton);
  end;
  ActiveIndex:=0;
  FillButtons;
end;

procedure TDlgGlyph.FillBmpList;
var
	f : TSearchRec;
  Prec : Integer;
begin
	BmpList.Clear;
  Prec := FindFirst(ActiveDirectory + '*.bmp', faArchive, F);
  while (Prec=0) do
  begin
    if (EditFiltrer.Text = '') or (Pos(UpperCase(EditFiltrer.Text), UpperCase(F.Name)) > 0) then
      BmpList.Add(LowerCase(F.Name));
    Prec := FindNext(F);
  end;
  ActiveIndex:=0;
  FillButtons;
end;

procedure TDlgGlyph.FillButtons;
var
	ABtn : TBitBtn;
  ABmp : TBitmap;
  i,n,ng : Integer;
begin
  if BtnList.Count<=0 then
    Exit;
	ABtn:=TBitBtn(BtnList.Items[0]);
 	ABtn.Glyph.Assign(Nil);
	ABtn.Caption:='(vide/empty)';
	for i:=0 to HeightCount * WidthCount - 2 do
  begin
  	n:=i+ActiveIndex;
  	ABtn:=TBitBtn(BtnList.Items[i+1]);
  	if (n>=BmpList.Count) then
    begin
    	ABtn.Glyph.Assign(Nil);
			ABtn.Caption:='(vide/empty)';
      ABtn.Enabled:=False;
    end
    else
    begin
    	ABtn.Enabled:=True;
      if CompareText(ExtractFileExt(BmpList[n]), '.bmp') = 0 then
      begin
        ABmp:=TBitmap.Create;
        try
          ABmp.LoadFromFile(ActiveDirectory + BmpList[n]);
          ng:=1;
          if ABmp.Width mod ABmp.Height=0 then
          begin
            ng:=ABmp.Width div ABmp.Height;
            if ng>4 then
              ng:=1;
          end;
        finally
          ABmp.Free;
        end;
      end
      else
        ng := 1;
      try
        ABtn.Glyph.LoadFromFile(ActiveDirectory + BmpList[n]);
      except
        ABtn.Glyph.Assign(nil);
        ABtn.Enabled:=False;
      end;
      ABtn.NumGlyphs:=ng;
      ABtn.Caption := Copy(BmpList[n], 1, Pos('.',BmpList[n])-1);
    end;
  end;
end;

procedure TDlgGlyph.BtnNextClick(Sender: TObject);
begin
	if ActiveIndex + HeightCount*WidthCount <= BmpList.Count then
  begin
  	Inc(ActiveIndex,HeightCount * WidthCount - 1);
    FillButtons;
  end;
end;

procedure TDlgGlyph.BtnPriorClick(Sender: TObject);
begin
	ActiveIndex:=ActiveIndex - HeightCount * WidthCount + 1 ;
  if ActiveIndex<0 then
  	ActiveIndex:=0;
  FillButtons;
end;

procedure TDlgGlyph.BtnGlyphClick(Sender : TObject);
begin
  BtnFinal.Glyph.Assign(TBitBtn(Sender).Glyph);
  BtnFinal.NumGlyphs:=TBitBtn(Sender).NumGlyphs
end;

procedure TDlgGlyph.DirListBoxChange(Sender: TObject);
begin
	if not IgnoreClick then
  begin
		ActiveDirectory:=DirListBox.Directory;
	  if ActiveDirectory[Length(ActiveDirectory)]<>'\' then
	  	ActiveDirectory:=ActiveDirectory+'\';
    EditFiltrer.Text := '';
	  FillBmpList;
    EditFiltrer.SetFocus;
  end;
end;

procedure TDlgGlyph.CBEnglishClick(Sender: TObject);
begin
  if CBEnglish.Checked then
  begin
    Caption := 'Choose a button';
    BtnPrior.Caption:='Prior page';
    BtnNext.Caption:='Next page';
    BtnFinal.Caption:='Final choice';
    BtnCancel.Caption:='Cancel';
    BtnAbout.Caption:='About...';
    RGButtons.Caption:='Buttons (WxH)';
    LabelFiltrer.Caption := 'Filter';
  end
  else
  begin
    Caption := 'Choix d''un bouton';
    BtnPrior.Caption:='Page précédente';
    BtnNext.Caption:='Page suivante';
    BtnFinal.Caption:='Choix final';
    BtnCancel.Caption:='Annuler';
    BtnAbout.Caption:='A propos...';
    RGButtons.Caption:='Boutons (LxH)';
    LabelFiltrer.Caption := 'Filtrer';
  end;
end;

procedure TDlgGlyph.RGButtonsClick(Sender: TObject);
begin
  Case RGButtons.ItemIndex of
    0 : begin
      WidthCount:=6;
      HeightCount:=10;
    end;
    1 : begin
      WidthCount:=4;
      HeightCount:=10;
    end;
    2 : begin
      WidthCount:=2;
      HeightCount:=10;
    end;
    3 : begin
      WidthCount:=6;
      HeightCount:=5;
    end;
    4 : begin
      WidthCount:=4;
      HeightCount:=5;
    end;
    5 : begin
      WidthCount:=2;
      HeightCount:=5;
    end;
    else
    begin
      WidthCount:=4;
      HeightCount:=10;
    end;
  end;
  if IgnoreClick then Exit;

  CreateBtnList;
end;

procedure TDlgGlyph.BtnCloseAboutClick(Sender: TObject);
begin
  PanelAbout.Visible := False;
end;

procedure TDlgGlyph.BtnAboutClick(Sender: TObject);
begin
  PanelAbout.Align:=alClient;
  PanelAbout.BringToFront;
  PanelAbout.Visible:=True;
end;

procedure Register;
begin
	RegisterPropertyEditor(TypeInfo(TBitmap),TBitBtn,'Glyph',TGlyphProperty);
	RegisterPropertyEditor(TypeInfo(TBitmap),TSpeedButton,'Glyph',TGlyphProperty);
	RegisterPropertyEditor(TypeInfo(TBitmap),TObject,'Glyph',TGlyphProperty);
  {$IFDEF GZ_D4}
	(* RegisterPropertyEditor(TypeInfo(TBitmap),TMenuItem,'Bitmap',TGlyphProperty); *)
  {$ENDIF}
end;

procedure TDlgGlyph.EditFiltrerChange(Sender: TObject);
begin
  FillBmpList;
end;

procedure TDlgGlyph.Panel1Resize(Sender: TObject);
begin
  BtnCloseAbout.Left := (Panel1.Width - BtnCloseAbout.Width) div 2;
end;

end.
