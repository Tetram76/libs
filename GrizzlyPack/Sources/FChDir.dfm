object DlgChoixDirectory: TDlgChoixDirectory
  Left = 200
  Top = 108
  ActiveControl = DirectoryListBox1
  BorderStyle = bsDialog
  Caption = 'Choix d'#39'un r'#233'pertoire'
  ClientHeight = 253
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 429
    Height = 209
  end
  object BitBtn1: TBitBtn
    Left = 138
    Top = 220
    Width = 77
    Height = 29
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 222
    Top = 220
    Width = 77
    Height = 29
    TabOrder = 1
    Kind = bkCancel
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 36
    Width = 209
    Height = 132
    DirLabel = Label1
    FileList = FileListBox1
    IntegralHeight = True
    ItemHeight = 16
    TabOrder = 2
  end
  object FileListBox1: TFileListBox
    Left = 220
    Top = 36
    Width = 209
    Height = 160
    Enabled = False
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 3
  end
  object DriveComboBox1: TDriveComboBox
    Left = 8
    Top = 188
    Width = 209
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 421
    Height = 25
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    TabOrder = 5
    object Label1: TLabel
      Left = 5
      Top = 5
      Width = 412
      Height = 13
      AutoSize = False
      Caption = 'E:\Grizzly\ExtComps\D7'
      Transparent = True
    end
  end
end
