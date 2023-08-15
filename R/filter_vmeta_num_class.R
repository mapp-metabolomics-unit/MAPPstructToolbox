#' @eval get_description('filter_vmeta_num')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_vmeta_num(mode='above', level=10, comparison='>', factor_name='QC')
#' M = model_apply(M, D)
#' @export filter_vmeta_num
filter_vmeta_num = function(mode='exact', level, comparison='=', factor_name, ...) {
    out = struct::new_struct('filter_vmeta_num',
        mode = mode,
        level = level,
        comparison = comparison,
        factor_name = factor_name,
        ...
    )
    return(out)
}

.filter_vmeta_num <- setClass(
    "filter_vmeta_num",
    contains = c('model'),
    slots = c(mode = 'enum',
              level = 'numeric',
              comparison = 'enum',
              factor_name = 'entity',
              filtered = 'DatasetExperiment'
    ),
    prototype = list(type = 'filter',
                     name = 'Filter by numeric variable metadata',
                     description = paste0('The data is filtered based on numeric variable metadata ',
                                         'using the specified comparison and level. '),
                     predicted = 'filtered',
                     .params = c('mode', 'level', 'comparison', 'factor_name'),
                     .outputs = c('filtered'),

                     mode = enum(name = 'Mode of action',
                                 description = c(
                                     "exact" = 'Samples with exact numeric value are retained.',
                                     "above" = 'Samples with numeric values above the threshold are retained.',
                                     "below" = 'Samples with numeric values below the threshold are retained.'
                                 ),
                                 type = 'character',
                                 allowed = c('exact', 'above', 'below'),
                                 value = 'exact',
                                 max_length = 1
                     ),

                     level = numeric(name = 'Numeric threshold',
                                     description = 'The numeric value for filtering.',
                                     type = 'numeric',
                                     value = NA
                     ),

                     comparison = enum(name = 'Comparison operator',
                                       description = c(
                                           "=" = 'Equal to',
                                           ">" = 'Greater than',
                                           "<" = 'Less than'
                                       ),
                                       type = 'character',
                                       allowed = c('=', '>', '<'),
                                       value = '=',
                                       max_length = 1
                     ),

                     factor_name = entity(name = 'Factor name',
                                          description = 'The factor name for filtering.',
                                          type = 'character',
                                          value = character(0)
                     )
    )
)

#' @export
#' @template model_apply
setMethod(f = "model_apply",
          signature = c("filter_vmeta_num", "DatasetExperiment"),
          definition = function(M, D) {
              opt = param_list(M)
              vmeta = D$variable_meta
              x = vmeta[[opt$factor_name]]
              if (opt$comparison == '=') {
                  out = ifelse(opt$mode == 'exact', x == opt$level, NA)
              } else if (opt$comparison == '>') {
                  out = ifelse(opt$mode == 'above', x > opt$level, NA)
              } else if (opt$comparison == '<') {
                  out = ifelse(opt$mode == 'below', x < opt$level, NA)
              }
              D = D[, !out]
              # drop excluded levels from factors
              D$variable_meta = droplevels(D$variable_meta)
              output_value(M, 'filtered') = D
              return(M)
          }
)

#' @export
#' @template model_train
setMethod(f = "model_train",
          signature = c("filter_vmeta_num", "DatasetExperiment"),
          definition = function(M, D) {
              M = model_apply(M, D)
          }
)

#' @export
#' @template model_predict
setMethod(f = "model_predict",
          signature = c("filter_vmeta_num", "DatasetExperiment"),
          definition = function(M, D) {
              M = model_apply(M, D)
          }
)
