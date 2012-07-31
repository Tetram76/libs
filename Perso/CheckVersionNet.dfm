object frmVerifUpgrade: TfrmVerifUpgrade
  Left = 497
  Top = 332
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Nouvelle version'
  ClientHeight = 151
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 48
    Width = 48
    Height = 13
    Cursor = crHandPoint
    Caption = 'Voir le site'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label1Click
    OnMouseEnter = Label1MouseEnter
    OnMouseLeave = Label1MouseLeave
  end
  object Label2: TLabel
    Left = 136
    Top = 48
    Width = 167
    Height = 13
    Cursor = crHandPoint
    Caption = 'T'#233'l'#233'charger le fichier de mise '#224' jour'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label2Click
    OnMouseEnter = Label1MouseEnter
    OnMouseLeave = Label1MouseLeave
  end
  object Label3: TLabel
    Left = 0
    Top = 0
    Width = 210
    Height = 13
    Caption = 'Votre version de ce programme est obsol'#232'te.'
  end
  object Panel1: TPanel
    Left = 0
    Top = 79
    Width = 303
    Height = 41
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object Button1: TButton
      Left = 110
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 79
    Width = 303
    Height = 72
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Label4: TLabel
      Left = 0
      Top = 46
      Width = 290
      Height = 26
      Align = alBottom
      Caption = 
        'Cliquez sur "Fermer" pour fermer l'#39'application et proc'#233'der '#224' la ' +
        'mise '#224' jour manuelle.'
      WordWrap = True
    end
    object Button2: TButton
      Left = 62
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Continuer'
      Default = True
      ModalResult = 2
      TabOrder = 0
      WordWrap = True
    end
    object Button3: TButton
      Left = 166
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Fermer'
      ModalResult = 1
      TabOrder = 1
    end
  end
end
