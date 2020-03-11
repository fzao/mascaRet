!== Copyright (C) 2000-2020 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_VALIDA_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P2R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine VALIDA ( &
             SVRAI   , &
             QVRAI   , &
             UVRAI   , &
             ZVRAI   , &
             YVRAI   , &
             QFIXG   , &
             SGAUC   , &
             SDROI   , &
             X       , &
             COTR    , &
             T       , &
             ITEMP   , &
             NBSECT  , &
             IVALID  , &
         Impression  , & ! Flag d'impression
         UniteListing, & ! Unite logique fichier listing
           Erreur      &
                   )

!***********************************************************************
!  FONCTION :
!  --------
!
!      SUBROUTINE DE VALIDATION DU CODE MASCARET
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SVRAI    ! TR !  R ! SURFACE VRAIE LORS DE LA VALIDATION          !
! !  QVRAI    ! TR !  R ! DEBIT VRAI LORS DE LA VALIDATION             !
! !  UVRAI    ! TR !  R ! VITESSE VRAIE LORS DE LA VALIDATION          !
! !  ZVRAI    ! TR !  R ! SURFACE LIBRE VRAIE LORS DE LA VALIDATION    !
! !  YVRAI    ! TR !  R ! TIRANT D'EAU VRAI LORS DE LA VALIDATION      !
! !  QFIXG    !  R !  D ! DEBIT IMPOSE A L'AMONT                       !
! !  SGAUC    !  R !  D ! SECTION MOUILLE A GAUCHE DU BIEF             !
! !  SDROI    !  R !  D ! SECTION MOUILLE A DROITE DU BIEF             !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  COTR     ! TR !  D ! COTE DU FOND                                 !
! !  T        !  R !  D ! INSTANT DU CALCUL                            !
! !  ITEMP    !  I !    !                                              !
! !  NBSECT   !  I !  D ! NOMBRE DE POINTS DU DOMAINE                  !
! !  IVALID   !  I !  D ! NUMERO DU CAS DE VALIDATION                  !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!*****************************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR
   use M_RIEMAN_I   ! Interface du sous-programme RIEMAN
   use M_SEUIL_I    ! Interface du sous-programme SEUIL

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:)    , intent(  out) :: SVRAI,QVRAI,UVRAI,ZVRAI,YVRAI
   real(DOUBLE),                   intent(in)    :: QFIXG
   real(DOUBLE),                   intent(in)    :: SGAUC
   real(DOUBLE),                   intent(inout) :: SDROI
   real(DOUBLE), dimension(:)    , intent(in)    :: X,COTR
   real(DOUBLE),                   intent(in)    :: T
   integer     ,                   intent(in)    :: ITEMP
   integer     ,                   intent(in)    :: NBSECT,IVALID
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   Type (ERREUR_T)               , intent(inout) :: Erreur

   end subroutine VALIDA

   end interface

end module M_VALIDA_I
