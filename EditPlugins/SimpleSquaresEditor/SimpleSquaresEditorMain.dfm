inherited FrameSimpleSquaresEditor: TFrameSimpleSquaresEditor
  Width = 649
  Height = 457
  object SplitterSquares: TSplitter
    Left = 150
    Top = 0
    Height = 457
    MinSize = 100
  end
  object SquaresContainer: TCategoryButtons
    Left = 0
    Top = 0
    Width = 150
    Height = 457
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonWidth = 0
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions, boBoldCaptions, boCaptionOnlyBorder]
    Images = SquaresImages
    Categories = <
      item
        Caption = 'Composants'
        Color = 15252386
        Collapsed = False
        Items = <
          item
            Caption = 'Nouveau'
            Hint = 'Cr'#233'er un nouveau composant de case'
            ImageIndex = 0
            OnClick = ButtonNewComponentClick
          end
          item
            Caption = 'Supprimer'
            Hint = 'Supprimer le composant s'#233'lectionn'#233
            ImageIndex = 1
            OnClick = ButtonDeleteComponentClick
          end>
      end
      item
        Caption = 'Terrains'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Effets'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Objets'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Obstacles'
        Color = 15252386
        Collapsed = False
        Items = <>
      end
      item
        Caption = 'Code'
        Color = 15252386
        Collapsed = True
        Items = <
          item
            Caption = 'Code source'
            Hint = 'Enregistrer le code source Delphi sous...'
            ImageIndex = 2
            OnClick = ButtonSaveSourceClick
          end>
      end>
    Color = clWhite
    RegularButtonColor = 15660791
    SelectedButtonColor = 15711942
    ShowHint = True
    TabOrder = 0
    OnButtonClicked = SquaresContainerButtonClicked
  end
  inline SquareEditor: TFrameEditSimpleSquare
    Left = 153
    Top = 0
    Width = 496
    Height = 457
    Align = alClient
    Enabled = False
    TabOrder = 1
    TabStop = True
    inherited PanelCommon: TPanel
      Width = 496
      inherited ButtonNewImage: TSpeedButton
        Left = 464
      end
      inherited ButtonRemoveImage: TSpeedButton
        Left = 464
      end
      inherited ButtonUpImage: TSpeedButton
        Left = 464
      end
      inherited ButtonDownImage: TSpeedButton
        Left = 464
      end
      inherited ListBoxImages: TListBox
        Width = 201
      end
    end
  end
  object SquaresImages: TImageList
    Height = 30
    Width = 30
    Left = 88
    Top = 232
    Bitmap = {
      494C01010300040004001E001E00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000780000001E00000001002000000000004038
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000008740D000874
      0D0008740D0008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000008740D002EB9
      5A002EB95A0008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000632DE000632DE000000000000000000000000000000
      0000000000000000000000000000000000000000000097433F0097433F009743
      3F0097433F00BD9A9A00BD9A9A00BD9A9A00BD9A9A00BD9A9A00BD9A9A00BD9A
      9A00BD9A9A00BD9A9A00BD9A9A00BD9A9A0097433F0097433F0097433F009743
      3F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000E81AA000E81AA000E81AA000E81
      AA000E81AA000E81AA000E81AA000E81AA000E81AA000000000008740D0032BF
      56002EB95A0008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000632DE000632DE000632DE000000000000000000000000000000
      00000000000000000000000000000000000097433F00C7696800CF656500CB64
      6400C45F5F00C7757400E8E3E200D2CBCB00DAC1C000DEDAD900F3F0EE00F5F2
      F000EBE7E600EBE7E600EBE7E600C97D7C00932A2A0098373600B6565600C15E
      5E0097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000E81AA000E81AA001AA7D1001AA7D1001DAED70022C0
      E70042C5E6006EDCF00052BCD90058B3CB00098AB7000E81AA0008740D0035C2
      580032BF560008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000632
      DE000632DE000632DE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000632DE000632DE000632DE00000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6A00CF656500CC67
      6700C6606100B2555400E8E3E2009229290092292900C7696800F1EDEC00FEFE
      FE00F9F9F700F8F7F600F8F7F600C97D7C009127270098373600B6565600C563
      620097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E81AA000E97C50076D9E70067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA0008740D0039C6
      5C0035C2580008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000632
      DE000632DE000632DE000632DE00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000632DE000632
      DE000632DE000632DE0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CC67
      6700C55F5F00B85A5A00E8E3E2009229290092292900C56D6B00E4DFDE00FBFA
      F900FBFAF900FDFDFC00FDFEFE00C97D7C009127270098373600B6565600C562
      620097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA0008740D0008740D0008740D0008740D0008740D0008740D0040CC
      650039C65C0008740D0008740D0008740D0008740D0008740D0008740D000000
      0000000000000000000000000000000000000000000000000000000000000632
      DE000632DE000632DE000632DE000632DE000000000000000000000000000000
      000000000000000000000000000000000000000000000632DE000632DE000632
      DE000632DE000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CC67
      6700C55F5F00BA5B5B00E8E3E2009229290092292900C56D6B00CFCECE00F1ED
      EC00F8F7F600FEFEFE00FDFEFE00C97D7C009127270098373600B6565600C562
      620097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA0008740D0055DB860055DB860052DA7F004ED779004BD5740044D0
      6B0040CC65003CC9600035C2580032BF560032BF56002EB95A0008740D000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000632DE000632DE000632DE000632DE000632DE0000000000000000000000
      0000000000000000000000000000000000000632DE000632DE000632DE000632
      DE00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CC67
      6700C55F5F00BA5B5B00E8E3E2009229290092292900C56D6B00D7B7B600E2DD
      DC00F1EDEC00FEFEFE00FDFEFE00C97D7C009127270098373600B6565600C562
      620097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA0008740D0055DB860055DB860055DB860052DA7F004ED779004BD5
      740044D06B0040CC65003CC9600039C65C0035C2580032BF560008740D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000335FA000632DE000632DE000632DE000632DE00000000000000
      00000000000000000000000000000632DE000632DE000632DE000632DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CC67
      6700C55F5F00BD5D5D00E8E3E2009229290092292900C56D6B00BD9A9A00DAC1
      C000E2DDDC00FDFEFE00FDFEFE00C97D7C009127270097323200B6565600C562
      620097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA0008740D0005710A0005710A0005710A0008740D0008740D0050D9
      7C004BD5740008740D0005710A0005710A0005710A0008740D0005710A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000632DE000632DE000632DE000632DE000000
      0000000000000632DE000632DE000632DE000632DE000632DE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CC67
      6700C55F5F00BA5B5B00E8E3E200E8E3E200E8E3E200E8E3E200E8E3E200E8E3
      E200E8E3E200E8E3E200E8E3E200C97D7C0096313100973E3B00B7575700C561
      610097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA0008740D0055DB
      860052DA7F0008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000632DE000632DE000632DE000632
      DE000632DE000632DE000632DE000632DE000632DE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00C46B6900CF656500CD66
      6600CB646400C5616100C8777600C48E8E00C2939300C48E8E00C8898900CB83
      8300CB838300CB838300C7757400C55F5F00C45F5F00C55F5F00CB646400C560
      600097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB009BF8FD0093F3FD0093F3FD0097F4FD00BAF7
      FB00C4F8FB00C4F8FB00C4F8FB00ADF7FC0077DAE9001DAED70008740D0055DB
      860055DB860008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000632DE000632DE000335
      FA000632DE000335FA000632DE000632DE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500C45F
      5F00C6636300C66E6C00C46B6A00CA686700C9686800C9686800C9686800CA68
      6700CA686700CB676700C46B6A00C6737200C8777600C6737200CC676700C15E
      5E0097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB00BAF7FB00BFF6FA0077DAE90052BCD9001AA7D100109A
      C7000B93C20017A1CC0058B3CB0052BCD90076D4DF0076D8E50008740D0055DB
      860055DB860008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000632DE000632
      DE000632DE000335FA000632DE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500CEA3
      A100CEA3A100D2A6A400D2A6A400D2A6A400D2A6A400D2A6A400D2A6A400D2A6
      A400D2A6A400D2A6A400D2A6A400D2A6A400D2A6A400D2A6A400C6706E00CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000098AB70075D2D90058B3CB0047BEDE0022C0E70022C9F0002CD8
      FD005FE2FA0074EEFE0077DAE90052BCD90017A1CC00088CB90008740D0055DB
      860055DB860008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000632DE000632DE000335
      FA000632DE000632DE000335FA000335FA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DCC4
      C300FEFDFD00FEFDFD00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFDFD00FDFCFC00DEC7C500C7696800CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C86B20052BCD90086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA0008740D000874
      0D0008740D0008740D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000632DE000632DE000335FA000632
      DE000335FA000335FA000335FA000632DE000335FA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DFC8
      C700FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DFC8C700C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088CBA0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA000B93C2000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000335FA000335FA000335FA000632DE000335
      FA0000000000000000000335FA000335FA000335FA000335FA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00CECDCD00CECDCD00CECDCD00CECDCD00CECDCD00CECD
      CD00CECDCD00CECDCD00CECDCD00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA000B93C2000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000335FA000335FA000632DE000335FA000335FA000000
      00000000000000000000000000000335FA000335FA000335FA000335FA000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA000B93C2000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000335FA000335FA000335FA000335FA000335FA00000000000000
      000000000000000000000000000000000000000000000335FA000335FA000335
      FA00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA000B93C2000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000335FA000335FA000335FA000335FA000335FA0000000000000000000000
      00000000000000000000000000000000000000000000000000000335FA000335
      FA000335FA000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00CECDCD00CECDCD00CECDCD00CECDCD00CECDCD00CECD
      CD00CECDCD00CECDCD00CECDCD00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0075DCEB0086F3FE0067EBFD0033DAFC0022D3FB002CD8
      FD005FE2FA007DF0FE0093F3FD006EDCF00047BEDE00109CCA000B93C2000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000335
      FA000335FA000335FA000335FA000335FA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000335FA000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0077DAE900ADF7FC00ADF7FC00A4F7FC00ADF7FC009BF8
      FD009BF8FD009BF8FD009BF8FD008EF2FD0069DEF4001FB7DF000E97C5000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000335FA000335
      FA000335FA000335FA000335FA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB00BFF6FA00C4F8FB00C4F8FB00C4F8FB00BAF7FB00A4F7
      FC009BF8FD009BF8FD009BF8FD009BF8FD0093F3FD0074EEFE003CCCEE000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000335FA000335
      FA000335FA000335FA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DEC7
      C500FEFEFE00FEFEFE00CECDCD00CECDCD00CECDCD00CECDCD00CECDCD00CECD
      CD00CECDCD00CECDCD00CECDCD00FEFEFE00FEFEFE00DEC7C500C5636200CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000088DBB0066ADC000C4F8FB00C4F8FB00C4F8FB00C4F8FB00ADF7
      FC009BF8FD009BF8FD009BF8FD009BF8FD009BF8FD0086F3FE001AA7D1000E81
      AA00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000335FA000335
      FA000335FA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097433F00CF656500CF656500DAC1
      C000FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DFC8C700CF656500CF65
      650097433F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000088DBB000A91BF0075D2D90075D2D900A4F7FC0077DA
      E90077DAE90077DAE90075DCEB0052BCD90052BCD9000D85B0000E81AA000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000097433F0097433F00D7B7
      B600FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00DFC8C70097433F009743
      3F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000088DBB00088DBB00088DBB00088D
      BB00088DBB00088DBB00088DBB00088DBB00088DBB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000780000001E0000000100010000000000E00100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFFC000000000
      FFFFFFFFFFFFFFFFFFFFFFC000000000FFFFFFFFFFFFFFFFFFFFFFC000000000
      FFFFC3FFFFFFFFFFFFFFFFC000000000FFFFC3FFFFFFFCFF80000FC000000000
      FF0043FFFFFFF8FF000007C000000000FC0003FFE3FFF1FF000007C000000000
      F80003FFE1FFC3FF000007C000000000F800001FE0FF87FF000007C000000000
      F800001FF07F0FFF000007C000000000F800001FF83E1FFF000007C000000000
      F800001FFE183FFF000007C000000000F80003FFFF007FFF000007C000000000
      F80003FFFF80FFFF000007C000000000F80003FFFFC1FFFF000007C000000000
      F80003FFFF80FFFF000007C000000000F80003FFFF007FFF000007C000000000
      F8000FFFFE0C3FFF000007C000000000F8000FFFFC1E1FFF000007C000000000
      F8000FFFF83F8FFF000007C000000000F8000FFFF07FC7FF000007C000000000
      F8000FFFE0FFF7FF000007C000000000F8000FFFC1FFFFFF000007C000000000
      F8000FFFC3FFFFFF000007C000000000F8000FFFC7FFFFFF000007C000000000
      FC001FFFFFFFFFFF80000FC000000000FF007FFFFFFFFFFFFFFFFFC000000000
      FFFFFFFFFFFFFFFFFFFFFFC000000000FFFFFFFFFFFFFFFFFFFFFFC000000000
      FFFFFFFFFFFFFFFFFFFFFFC00000000000000000000000000000000000000000
      000000000000}
  end
  object SaveSourceDialog: TSaveDialog
    DefaultExt = 'fnd'
    Filter = 'Source FunDelphi (*.fnd)|*.fnd'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Enregistrer le code source FunDelphi sous...'
    Left = 88
    Top = 264
  end
end