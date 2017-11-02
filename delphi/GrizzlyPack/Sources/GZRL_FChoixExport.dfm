object FenExportDonnees: TFenExportDonnees
  Left = 158
  Top = 53
  BorderStyle = bsDialog
  Caption = 'Choix du format d'#39'export'
  ClientHeight = 346
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 276
    Height = 305
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object Label4: TLabel
      Left = 16
      Top = 12
      Width = 88
      Height = 13
      Caption = 'Format d'#39'export'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnCSV: TRadioButton
      Left = 28
      Top = 32
      Width = 133
      Height = 17
      Caption = 'Texte CSV ou assimil'#233
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = btnCSVClick
    end
    object btnTextConst: TRadioButton
      Left = 28
      Top = 56
      Width = 133
      Height = 17
      Caption = 'Texte '#224' taille constante'
      TabOrder = 1
      OnClick = btnTextConstClick
    end
    object PageControlFormat: TPageControl
      Left = 16
      Top = 84
      Width = 245
      Height = 209
      ActivePage = TabSheetCSV
      TabOrder = 2
      OnChange = PageControlFormatChange
      object TabSheetCSV: TTabSheet
        Caption = 'CSV'
        object Label1: TLabel
          Left = 8
          Top = 30
          Width = 154
          Height = 13
          Alignment = taRightJustify
          Caption = 'Caract'#232're s'#233'parateur de champs'
        end
        object Label2: TLabel
          Left = 7
          Top = 74
          Width = 135
          Height = 13
          Alignment = taRightJustify
          Caption = 'Caract'#232're(s) de saut de ligne'
        end
        object LabelQuote: TLabel
          Left = 28
          Top = 138
          Width = 95
          Height = 13
          Caption = 'Caract'#232're guillemets'
        end
        object Label3: TLabel
          Left = 4
          Top = 4
          Width = 141
          Height = 13
          Caption = 'Format excel par d'#233'faut'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
        end
        object CBQuote: TCheckBox
          Left = 8
          Top = 110
          Width = 149
          Height = 17
          Caption = 'Valeurs entre guillemets'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = CBQuoteClick
        end
        object EditQuote: TEdit
          Left = 128
          Top = 134
          Width = 21
          Height = 21
          MaxLength = 1
          TabOrder = 4
          Text = '"'
          OnChange = EditQuoteChange
        end
        object CBLineBreak: TComboBox
          Left = 148
          Top = 70
          Width = 81
          Height = 21
          ItemHeight = 13
          TabOrder = 2
          Text = '#13#10'
          OnExit = CBLineBreakExit
          Items.Strings = (
            '#13#10'
            '#13'
            '#10')
        end
        object EditQuoteASCII: TEdit
          Left = 152
          Top = 134
          Width = 37
          Height = 21
          TabOrder = 5
          Text = '#34'
          OnChange = EditQuoteASCIIChange
          OnExit = EditQuoteASCIIExit
        end
        object EditSeparator: TEdit
          Left = 168
          Top = 26
          Width = 21
          Height = 21
          MaxLength = 1
          TabOrder = 0
          Text = ';'
          OnChange = EditSeparatorChange
        end
        object EditSeparatorASCII: TEdit
          Left = 192
          Top = 26
          Width = 37
          Height = 21
          TabOrder = 1
          Text = '#59'
          OnChange = EditSeparatorASCIIChange
          OnExit = EditSeparatorASCIIExit
        end
      end
      object TabSheetConstText: TTabSheet
        Caption = 'Taille constante'
        ImageIndex = 1
        object Label5: TLabel
          Left = 8
          Top = 8
          Width = 115
          Height = 13
          Caption = 'Taille des champs : '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 24
          Top = 36
          Width = 33
          Height = 13
          Caption = 'Texte :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 24
          Top = 56
          Width = 57
          Height = 13
          Caption = 'Num'#233'rique :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 24
          Top = 76
          Width = 29
          Height = 13
          Caption = 'Date :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 104
          Top = 36
          Width = 95
          Height = 13
          Caption = 'Longueur du champ'
        end
        object Label10: TLabel
          Left = 104
          Top = 56
          Width = 65
          Height = 13
          Caption = '21 caract'#232'res'
        end
        object Label11: TLabel
          Left = 104
          Top = 76
          Width = 122
          Height = 13
          Caption = '10 caract'#232'res jj/mm/aaaa'
        end
        object Label12: TLabel
          Left = 24
          Top = 96
          Width = 35
          Height = 13
          Caption = 'Heure :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label13: TLabel
          Left = 104
          Top = 96
          Width = 106
          Height = 13
          Caption = '8 caract'#232'res hh:mm:ss'
        end
        object Label14: TLabel
          Left = 24
          Top = 116
          Width = 59
          Height = 13
          Caption = 'Date heure :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label15: TLabel
          Left = 104
          Top = 116
          Width = 65
          Height = 13
          Caption = '19 caract'#232'res'
        end
        object Label16: TLabel
          Left = 24
          Top = 136
          Width = 45
          Height = 13
          Caption = 'Bool'#233'en :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 104
          Top = 136
          Width = 54
          Height = 13
          Caption = '1 caract'#232're'
        end
        object Label18: TLabel
          Left = 24
          Top = 156
          Width = 35
          Height = 13
          Caption = 'M'#233'mo :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label19: TLabel
          Left = 104
          Top = 156
          Width = 90
          Height = 13
          Caption = 'Non enregistr'#233
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
        end
      end
    end
  end
  object ExitPanel1: TExitPanel
    Left = 0
    Top = 305
    Width = 276
    Height = 41
    BtnHeight = 33
    BtnWidth = 120
    BtnOk.Caption = '&OK'
    BtnOk.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330200333333333333333333333333F33333333333
      A8013333344333333333333333388F3333333333A30133334224333333333333
      338338F3333333331301333422224333333333333833338F33333333A3013342
      222224333333333383333338F3333333020034222A22224333333338F338F333
      8F33333336013222A3A2224333333338F3838F338F333333A3013A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0200333333333A222433333333333338F338F3334F013333333333A222433333
      333333338F338F33C08133333333333A222433333333333338F338F302003333
      33333333A222433333333333338F338F00F03333333333333A22433333333333
      3338F38F010033333333333333A223333333333333338F830202333333333333
      333A333333333333333338330400333333333333333333333333333333333333
      6D01}
    BtnOk.ModalResult = 0
    BtnOk.Cancel = False
    BtnOk.Default = True
    BtnOk.Kind = bkCustom
    BtnCancel.Caption = '&Annuler'
    BtnCancel.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    BtnCancel.ModalResult = 2
    BtnCancel.Cancel = True
    BtnCancel.Default = False
    BtnCancel.Kind = bkCancel
    Spacing = 8
    Align = alBottom
    OnOkClick = ExitPanel1OkClick
  end
  object SD: TSaveDialog
    Filter = 
      'Fichiers CSV (*.csv)|*.csv|Fichiers texte (*.txt)|*.txt|Tous fic' +
      'hiers (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    OnTypeChange = SDTypeChange
    Left = 232
    Top = 12
  end
end
