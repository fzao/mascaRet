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

module M_INVMAT_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P2R0                EDF-CEREMA
! *********************************************************************

   interface

   SUBROUTINE INVMAT( A , L , N , K , Erreur )

!***********************************************************************
!   CODE MASCARET :  CALCUL l'INVERSE D'UNE MATRICE CARREE 2*2
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  A        ! R  !  A ! MATRICE PRODUIT DE B et C                    !
! !  B        ! R  !  A ! MATRICE                                      !
! !  C        ! R  !  A ! MATRICE                                      !
! !  N        ! TD !  A ! DIMENSION DES MATRICES                       !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
   use M_PRECISION
   use M_PARAMETRE_C          ! EPS_ONDE, W23, GPES
   use M_ERREUR_T    ! ERREUR

   IMPLICIT NONE

   real(DOUBLE)                , intent (inout) :: A(4),L(4)
   Integer    ,                  intent (in   ) :: K,N
   type(ERREUR_T)              , intent(inout) :: Erreur

   end subroutine INVMAT

   end interface

end module M_INVMAT_I
