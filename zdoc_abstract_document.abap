CLASS zdoc_abstract_document DEFINITION
  ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS display ABSTRACT
      RAISING zcx_return3.

    METHODS print ABSTRACT
      RAISING zcx_return3.

    METHODS get_pdf_xstring ABSTRACT
      RETURNING VALUE(pdf_xstring) TYPE xstring
      RAISING   zcx_return3.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZDOC_ABSTRACT_DOCUMENT IMPLEMENTATION.
ENDCLASS.
