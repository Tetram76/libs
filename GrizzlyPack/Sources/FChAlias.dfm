object DlgChoixAlias: TDlgChoixAlias
  Left = 138
  Top = 104
  ActiveControl = BtnOk
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Choix d'#39'une base de donn'#233'es'
  ClientHeight = 217
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 160
    Width = 27
    Height = 13
    Alignment = taRightJustify
    Caption = 'Alias'
  end
  object LabelBD: TLabel
    Left = 188
    Top = 4
    Width = 185
    Height = 29
    AutoSize = False
    Caption = 'LabelBD'
    WordWrap = True
  end
  object DriveComboBox1: TDriveComboBox
    Left = 4
    Top = 8
    Width = 181
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 0
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 4
    Top = 32
    Width = 181
    Height = 121
    Hint = '(c) Fr'#233'd'#233'ric GUILLIEN 05/1998'
    ItemHeight = 16
    TabOrder = 1
    OnChange = DirectoryListBox1Change
  end
  object BtnOk: TBitBtn
    Left = 106
    Top = 184
    Width = 81
    Height = 29
    TabOrder = 4
    Kind = bkOK
  end
  object BtnAnnuler: TBitBtn
    Left = 190
    Top = 184
    Width = 81
    Height = 29
    TabOrder = 5
    Kind = bkCancel
  end
  object CBAlias: TComboBox
    Left = 40
    Top = 156
    Width = 333
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnChange = CBAliasChange
  end
  object LBTables: TListBox
    Left = 188
    Top = 32
    Width = 185
    Height = 121
    ItemHeight = 13
    TabOrder = 2
  end
  object Database1: TDatabase
    SessionName = 'Default'
    Left = 16
    Top = 168
  end
end
