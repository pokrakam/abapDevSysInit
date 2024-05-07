* Example for include zdevsysinit_params. 
* 
* For simplicity, configuration is in code so it can be added to a new system
* by copy/paste.
* This is your personal config setup and should contain the customizations
* and kept locally or in a Gist or personal repo.
*
* On a new dev system: 
* - create include zdevsysinit_params
* - paste content of this example
* - adapt as needed
* - run zdevsysinit. 

*&---------------------------------------------------------------------*
*& Include zdevsysinit_params
*&---------------------------------------------------------------------*

" Your github (or other git host) user
github_user = `mygithubuser`.

" Add SSL certificates for these hosts to the SAP system
sslhosts = VALUE #(
    ( `github.com` )
    ( `github.io` )
).

" Change current user (DEVELOPER)
profile = VALUE #(
  firstname = 'John'
  lastname = 'Doe'
  email = 'someone@example.com'
  date_format = user_profile=>c_date_format-dmy_dot
  decimal_format = user_profile=>c_decimal_format-point
  hide_menu_picture = abap_true
  show_menu_tcodes = abap_true
).

" Git repositories to be pulled
repos = VALUE #(
  ( name    = `abapGit`   package = `$ZABAPGIT`   url = `https://github.com/abapgit/abapgit` )
  ( name    = `ABAP2XLSX` package = `$ZABAP2XLSX` url = `https://github.com/abap2xlsx/abap2xlsx` )
).
