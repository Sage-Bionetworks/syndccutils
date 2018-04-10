## Remove test data after tests are run. This avoids R CMD Check NOTEs caused by
## the following long file paths:
## syndccutils/tests/testthat/testdata/mock_datatable_files/datatables-css-0.0.0/datatables-crosstalk.css
## syndccutils/tests/testthat/testdata/mock_datatable_files/dt-core-1.10.16/css/jquery.dataTables.extra.css
## syndccutils/tests/testthat/testdata/mock_datatable_files/dt-core-1.10.16/css/jquery.dataTables.min.css

fs::dir_delete("testdata/")
