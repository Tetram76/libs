object DlgModifierMotDePasse: TDlgModifierMotDePasse
  Left = 577
  Top = 268
  ActiveControl = EditAncien
  BorderStyle = bsDialog
  Caption = 'Changer un mot de passe'
  ClientHeight = 157
  ClientWidth = 313
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
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 101
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 48
    Top = 24
    Width = 119
    Height = 13
    Alignment = taRightJustify
    Caption = 'Ancien mot de passe'
  end
  object Label2: TLabel
    Left = 36
    Top = 52
    Width = 131
    Height = 13
    Alignment = taRightJustify
    Caption = 'Nouveau mot de passe'
  end
  object Label3: TLabel
    Left = 92
    Top = 80
    Width = 74
    Height = 13
    Alignment = taRightJustify
    Caption = 'Confirmation'
  end
  object OKBtn: TBitBtn
    Left = 64
    Top = 116
    Width = 90
    Height = 33
    TabOrder = 0
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 160
    Top = 116
    Width = 89
    Height = 33
    TabOrder = 1
    Kind = bkCancel
  end
  object EditAncien: TEdit
    Left = 176
    Top = 20
    Width = 109
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    OnKeyPress = EditAncienKeyPress
  end
  object EditNouveau: TEdit
    Left = 176
    Top = 48
    Width = 109
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    OnKeyPress = EditNouveauKeyPress
  end
  object EditConfirmation: TEdit
    Left = 176
    Top = 76
    Width = 109
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
    OnKeyPress = EditConfirmationKeyPress
  end
end
