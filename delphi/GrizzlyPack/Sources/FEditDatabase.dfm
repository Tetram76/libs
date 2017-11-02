object FenEditDatabase: TFenEditDatabase
  Left = 476
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Choisissez l'#39'alias ou le chemin'
  ClientHeight = 121
  ClientWidth = 415
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
    Width = 289
    Height = 113
  end
  object Label1: TLabel
    Left = 36
    Top = 12
    Width = 73
    Height = 13
    Caption = 'Nom par d'#233'faut'
  end
  object Label2: TLabel
    Left = 64
    Top = 64
    Width = 47
    Height = 13
    Caption = 'Alias BDE'
  end
  object Label3: TLabel
    Left = 12
    Top = 92
    Width = 98
    Height = 13
    Caption = 'Recherche manuelle'
  end
  object LabelDefaultName: TLabel
    Left = 116
    Top = 12
    Width = 88
    Height = 13
    Caption = 'LabelDefaultName'
    Enabled = False
  end
  object Label4: TLabel
    Left = 8
    Top = 36
    Width = 104
    Height = 13
    Caption = 'Type base de donn'#233'e'
  end
  object AliasSelecter: TComboBox
    Left = 116
    Top = 60
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'AliasSelecter'
    OnChange = AliasSelecterChange
  end
  object PathSelecter: TEdit
    Left = 116
    Top = 88
    Width = 145
    Height = 21
    TabOrder = 1
    Text = 'PathSelecter'
    OnChange = PathSelecterChange
  end
  object btnPath: TButton
    Left = 264
    Top = 88
    Width = 21
    Height = 21
    Caption = 'ooo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = btnPathClick
  end
  object btnBDE: TButton
    Left = 240
    Top = 60
    Width = 45
    Height = 21
    Caption = 'BDE ...'
    TabOrder = 3
    OnClick = btnBDEClick
  end
  object Panel1: TPanel
    Left = 298
    Top = 0
    Width = 117
    Height = 121
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 4
    object btnOk: TBitBtn
      Left = 4
      Top = 4
      Width = 109
      Height = 29
      TabOrder = 0
      OnClick = ExitPanel1OkClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 4
      Top = 36
      Width = 109
      Height = 29
      TabOrder = 1
      Kind = bkCancel
    end
    object btnDefault: TBitBtn
      Left = 4
      Top = 80
      Width = 109
      Height = 29
      Caption = 'R'#233'tablir d'#233'faut'
      TabOrder = 2
      OnClick = btnDefaultClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00888888744788
        8888888887477478888888888488884888888888748888478888888878888884
        8888888888888884788877777777777777788888888888888888FFF4FFFFFFFF
        FFF8888448888884788888844488888478888884444888848888888444888847
        8888888884788848888888888747747888888888887447888888}
    end
  end
  object DriverSelecter: TComboBox
    Left = 116
    Top = 32
    Width = 169
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = DriverSelecterChange
    Items.Strings = (
      'Standard'
      'Microsoft Access'
      'Interbase')
  end
  object OpenDialog1: TOpenDialog
    Left = 28
    Top = 55
  end
end
