***INCLUDE MV45AFZA .

************************************************************************
*                                                                      *
* This include is reserved for user modifications                      *
*                                                                      *
* Forms for sales document processing                                  *
*                                                                      *
* The name of modification modules should begin with 'ZZ'.             *
*                                                                      *
************************************************************************

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_KOMKD                             *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the        *
*       communication workarea for product substitution.              *
*                                                                     *
*       This form is called from form KOMKD_KOMPD_FUELLEN.            *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_KOMKD.

*  KOMKD-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_KOMPD                             *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the        *
*       communication workarea for product substitution.              *
*                                                                     *
*       This form is called from form KOMKD_KOMPD_FUELLEN.            *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_KOMPD.

*  KOMPD-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_KOMKG                             *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the        *
*       communication workarea for product listing or exclusion.      *
*                                                                     *
*       This form is called from form KOMKG_KOMPG_FUELLEN.            *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_KOMKG.

*  KOMKG-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_KOMPG                             *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the        *
*       communication workarea for product listing or exclusion.      *
*                                                                     *
*       This form is called from form KOMKG_KOMPG_FUELLEN.            *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_MOVE_FIELD_TO_KOMPG.

*  KOMPG-zzfield = xxxx-zzfield2.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_REFRESH_DOCUMENT                                *
*---------------------------------------------------------------------*
*       It is always necessary to refresh user-specific data before   *
*       the next document will be processed.                          *
*       This can be done in this userexit.                            *
*       This userexit can be used to refresh user-specific data       *
*       when the processing of a sales document is finished           *
*       - after the document is saved                                 *
*       - when you leave the document processing with F3 or F15       *
*       It may be necessary to refresh user-specific data before      *
*       the next document will be processed.                          *
*                                                                     *
*      This form is called in dialog at the end of form               *
*      BELEG_INITIALISIEREN                                           *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_REFRESH_DOCUMENT.

* clear: zz45a.
* refresh: zzitemtab.

ENDFORM.
*eject
