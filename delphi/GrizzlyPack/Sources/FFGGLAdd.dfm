object DlgGenListAddItem: TDlgGenListAddItem
  Left = 438
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Nouvel '#233'l'#233'ment'
  ClientHeight = 199
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 4
    Top = 56
    Width = 345
    Height = 89
  end
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 345
    Height = 49
  end
  object LabelItem: TLabel
    Left = 24
    Top = 60
    Width = 89
    Height = 13
    Caption = 'Nouvel '#233'l'#233'ment'
  end
  object LabelValue: TLabel
    Left = 24
    Top = 100
    Width = 111
    Height = 13
    Caption = 'Valeur de l'#39#233'l'#233'ment'
  end
  object LabelTitre: TLabel
    Left = 12
    Top = 12
    Width = 329
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Ajout d'#39'un '#233'l'#233'ment pour le groupe'
  end
  object LabelGroupName: TLabel
    Left = 12
    Top = 32
    Width = 329
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Nom du groupe'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EditItem: TEdit
    Left = 24
    Top = 76
    Width = 305
    Height = 21
    TabOrder = 0
  end
  object EditValue: TEdit
    Left = 24
    Top = 116
    Width = 305
    Height = 21
    TabOrder = 1
  end
  object BtnOk: TBitBtn
    Left = 92
    Top = 156
    Width = 81
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = BtnOkClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330200333333333333333333333333F33333333333
      EC003333344333333333333333388F3333333333CD0133334224333333333333
      338338F333333333B581333422224333333333333833338F33333333CF003342
      222224333333333383333338F3333333BA0034222A22224333333338F338F333
      8F333333CF003222A3A2224333333338F3838F338F33333302003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      1C01333333333A222433333333333338F338F33335013333333333A222433333
      333333338F338F33AE0033333333333A222433333333333338F338F304003333
      33333333A222433333333333338F338F00F03333333333333A22433333333333
      3338F38F010033333333333333A223333333333333338F838102333333333333
      333A33333333333333333833BE62333333333333333333333333333333333333
      0200}
    NumGlyphs = 2
  end
  object BtnCancel: TBitBtn
    Left = 180
    Top = 156
    Width = 81
    Height = 33
    TabOrder = 3
    Kind = bkCancel
  end
end
