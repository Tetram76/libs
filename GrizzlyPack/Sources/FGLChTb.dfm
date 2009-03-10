object GLDlgChoixTables: TGLDlgChoixTables
  Left = 200
  Top = 109
  ActiveControl = LBGroups
  BorderStyle = bsDialog
  Caption = 'Choix des tables sources'
  ClientHeight = 213
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 105
    Height = 13
    Caption = 'Table des groupes'
  end
  object Label2: TLabel
    Left = 132
    Top = 4
    Width = 111
    Height = 13
    Caption = 'Table des '#233'l'#233'ments'
  end
  object LBGroups: TListBox
    Left = 4
    Top = 20
    Width = 121
    Height = 149
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
  end
  object LBItems: TListBox
    Left = 132
    Top = 20
    Width = 121
    Height = 149
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
  end
  object BtnOk: TButton
    Left = 48
    Top = 176
    Width = 77
    Height = 29
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BtnCancel: TButton
    Left = 132
    Top = 176
    Width = 77
    Height = 29
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
end
