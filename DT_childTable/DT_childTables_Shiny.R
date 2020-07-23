library(shiny)
library(DT)
library(jsonlite)

## data ####
dat <- data.frame(
  Sr = c(1.5, 2.3),
  Description = c("A - B", "X - Y")
)
## details of row 1
subdat1 <- data.frame(
  Chromosome = "chr18", 
  SNP = "rs2",
  stringsAsFactors = FALSE
)
## details of row 2
subdat2 <- data.frame(
  Chromosome = c("chr19","chr20"), 
  SNP = c("rs3","rs4"), 
  stringsAsFactors = FALSE
)
## merge the row details
subdats <- lapply(list(subdat1, subdat2), purrr::transpose)
## dataframe for the datatable
Dat <- cbind(" " = "&oplus;", dat, details = I(subdats))

## the callback ####
registerInputHandler("x.child", function(x, ...) {
  fromJSON(toJSON(x, auto_unbox = TRUE, null = "null"), simplifyDataFrame = FALSE)
}, force = TRUE)

callback = JS(
  "var expandColumn = table.column(0).data()[0] === '&oplus;' ? 0 : 1;",
  "table.column(expandColumn).nodes().to$().css({cursor: 'pointer'});",
  "",
  "// send selected columns to Shiny",
  "var tbl = table.table().node();",
  "var tblId = $(tbl).closest('.datatables').attr('id');",
  "table.on('click', 'td:not(:nth-child(' + (expandColumn+1) + '))', function(){",
  "  setTimeout(function(){",
  "    var indexes = table.rows({selected:true}).indexes();",
  "    var indices = Array(indexes.length);",
  "    for(var i = 0; i < indices.length; ++i){",
  "      indices[i] = indexes[i];",
  "    }",
  "    Shiny.setInputValue(tblId + '_rows_selected', indices);",
  "  },0);",
  "});",
  "",
  "// Format the nested table into another table",
  "var format = function(d, childId){",
  "  if (d != null) {",
  "    var html = ", 
  "      '<table class=\"display compact\" id=\"' + childId + '\"><thead><tr>';",
  "    for (var key in d[d.length-1][0]) {",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'papayawhip');",
  "    }",
  "  } else {",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'lemonchiffon');",
  "    }",
  "  }",
  "};",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for (var i = 0; i < d[n].length; i++) {",
  "    var datarow = $.map(d[n][i], function(value, index){",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  var subtable = $(id).DataTable({",
  "                   'data': dataset,",
  "                   'autoWidth': true,",
  "                   'deferRender': true,",
  "                   'info': false,",
  "                   'lengthChange': false,",
  "                   'ordering': d[n].length > 1,",
  "                   'paging': false,",
  "                   'scrollX': false,",
  "                   'scrollY': false,",
  "                   'searching': false,",
  "                   'sortClasses': false,",
  "                   'rowCallback': rowCallback,",
  "                   'headerCallback': headerCallback,",
  "                   'select': {style: 'multi'},",
  "                   'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "                 });",
  "};",
  "",
  "var nrows = table.rows().count();",
  "var nullinfo = Array(nrows);",
  "for(var i = 0; i < nrows; ++i){",
  "  nullinfo[i] = {child : 'child-'+i, selected: null};",
  "}",
  "Shiny.setInputValue(tblId + '_children:x.child', nullinfo);",
  "var sendToR = function(){",
  "  var info = [];",
  "  setTimeout(function(){",
  "    for(var i = 0; i < nrows; ++i){",
  "      var childId = 'child-' + i;",
  "      var childtbl = $('#'+childId).DataTable();",
  "      var indexes = childtbl.rows({selected:true}).indexes();",
  "      var indices;",
  "      if(indexes.length > 0){",
  "        indices = Array(indexes.length);",
  "        for(var j = 0; j < indices.length; ++j){",
  "          indices[j] = indexes[j];",
  "        }",
  "      } else {",
  "        indices = null;",
  "      }",
  "      info.push({child: childId, selected: indices});",
  "    }",
  "    Shiny.setInputValue(tblId + '_children:x.child', info);",
  "  }, 0);",
  "}",
  "$('body').on('click', '[id^=child-] td', sendToR);",
  "",
  "table.on('click', 'td.details-control', function () {",
  "  var td = $(this),",
  "      row = table.row(td.closest('tr'));",
  "  if (row.child.isShown()) {",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "    sendToR();",
  "  } else {",
  "    var childId = 'child-' + row.index();",
  "    row.child(format(row.data(), childId)).show();",
  "    row.child.show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data(), childId);",
  "  }",
  "});")

## shiny app ####
ui <- fluidPage(
  DTOutput("table"),
  verbatimTextOutput("info")
)

server <- function(input, output){
  output[["table"]] <- renderDT({
    datatable(Dat, callback = callback, escape = -2, 
              extensions = "Select", selection = "none",
              options = list(
                select = list(style = "multi", selector = ".selectable"),
                autoWidth = FALSE,
                columnDefs = list(
                  list(className = "selectable dt-center", 
                       targets = c(0, 2:ncol(Dat))),
                  list(visible = FALSE, targets = ncol(Dat)),
                  list(orderable = FALSE, className = 'details-control', 
                       width = "10px", targets = 1),
                  list(className = "dt-center", targets = "_all")
                )
              )
    )
  }, server = FALSE)
  
  output[["info"]] <- renderText({
    text <- sprintf("Selected row(s) of main table: %s\n", 
                    input[["table_rows_selected"]])
    text <- c(text, "Selected row(s) of children:\n")
    text <- c(text, paste0(input[["table_children"]], collapse="\n"))
    text
  })
  
  observe({
    print("------------------")
    print(input[["table_rows_selected"]])
    print("------------------")
    print(input[["table_children"]])
  })
}

shinyApp(ui, server)
