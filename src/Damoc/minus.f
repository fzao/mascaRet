C                       ********************
                        SUBROUTINE MINUSMASC
C                       ********************
     *(CHAINE)
C
C***********************************************************************
C DAMOCLES VERSION 5.0       06/10/97     A. DESITTER (BRISTOL)
C                            30/01/92    J-M HERVOUET (LNH) 30 87 80 18
C***********************************************************************
C
C FONCTION : TRANSFORME LES MAJUSCULES D'UNE CHAINE DE CARACTERES EN
C            MINUSCULES
C
C----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | CHAINE         |<-->| CHAINE DE CARACTERES A MODIFIER
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*26 STMAJ,STMIN
      CHARACTER*(*) CHAINE
C
      INTEGER I,IPOS
C
      INTRINSIC LEN,INDEX
C
      DATA STMAJ /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA STMIN /'abcdefghijklmnopqrstuvwxyz'/
C
C----------------------------------------------------------------------
C
      DO 10 I=1,LEN(CHAINE)
C
      IPOS=INDEX(STMAJ,CHAINE(I:I))
      IF(IPOS.NE.0) CHAINE(I:I)=STMIN(IPOS:IPOS)
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
