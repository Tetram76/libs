object DlgIdentification: TDlgIdentification
  Left = 245
  Top = 108
  ActiveControl = EditOperateur
  BorderStyle = bsDialog
  Caption = 'Identification'
  ClientHeight = 153
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
    Height = 97
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 34
    Top = 48
    Width = 142
    Height = 13
    Alignment = taRightJustify
    Caption = 'Identifiant de l'#39'utilisateur'
  end
  object Label2: TLabel
    Left = 101
    Top = 76
    Width = 75
    Height = 13
    Alignment = taRightJustify
    Caption = 'Mot de passe'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 281
    Height = 13
    AutoSize = False
    Caption = 'Application'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object OKBtn: TBitBtn
    Left = 63
    Top = 112
    Width = 90
    Height = 33
    TabOrder = 2
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 159
    Top = 112
    Width = 90
    Height = 33
    TabOrder = 3
    Kind = bkCancel
  end
  object EditOperateur: TEdit
    Left = 184
    Top = 44
    Width = 101
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    OnKeyPress = EditOperateurKeyPress
  end
  object EditPasse: TEdit
    Left = 184
    Top = 72
    Width = 101
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    OnKeyPress = EditPasseKeyPress
  end
end
