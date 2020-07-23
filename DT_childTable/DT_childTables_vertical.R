library(DT)

##~~ vertical subtable ~~##

## data
dat <- data.frame(
  Sr = c(1, 2),
  Description = c("A - B", "X - Y")
)

## row details
details <- list(list(Chromosome = "chr18", SNP = "rs2"), 
                list(Chromosome = "chr19", SNP = "rs3"))
# or 
details <- data.frame(
  Chromosome = c("chr18", "chr19"),
  SNP = c("rs2", "rs3"),
  stringsAsFactors = FALSE
) %>% purrr::transpose()

## dataframe for datatable
Dat <- cbind(" " = "&oplus;", dat, details = I(details))

## the callback
callback = JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "var format = function (d) {",
  "    var result = '<div><table style=\"background-color:#fadadd\">';", 
  "    for(var key in d[d.length-1]){",
  "      result += '<tr style=\"background-color:#fadadd\"><td><b>' + key +", 
  "                '</b>:</td><td>' + d[4][key] + '</td></tr>';",
  "    }",
  "    result += '</table></div>';",
  "    return result;",
  "}",
  "table.on('click', 'td.details-control', function(){",
  "  var td = $(this),",
  "      row = table.row(td.closest('tr'));",
  "  if (row.child.isShown()) {",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    row.child(format(row.data())).show();",
  "    td.html('&CircleMinus;');",
  "  }",
  "});")

## datatable
datatable(Dat, callback = callback, escape = -2,
          options = list(
            columnDefs = list(
              list(className = "dt-center", targets = 2:ncol(Dat)),
              list(visible = FALSE, targets = ncol(Dat)),
              list(orderable = FALSE, className = 'details-control', targets = 1)
            )
          ))
