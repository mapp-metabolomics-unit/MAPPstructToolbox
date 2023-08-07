#' @eval get_description('filter_vmeta')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_vmeta(mode='exclude',levels='QC',factor_name='QC')
#' M = model_apply(M,D)
#' @export filter_vmeta
filter_vmeta = function(mode='include',levels,factor_name,...) {
    out=struct::new_struct('filter_vmeta',
        mode=mode,
        levels=levels,
        factor_name=factor_name,
        ...)
    return(out)
}

.filter_vmeta<-setClass(
    "filter_vmeta",
    contains = c('model'),
    slots=c(mode='enum',
        levels='entity',
        factor_name='entity',
        filtered='DatasetExperiment'
    ),
    prototype=list(type = 'filter',
        name='Filter by variable meta data',
        description=paste0('The data is filtered by so that the named levels ',
        'of a factor are included/excluded from the dataset. '),
        predicted = 'filtered',
        .params=c('mode','levels','factor_name'),
        .outputs=c('filtered'),

        mode=enum(name='Mode of action',
            description=c(
                "include" = 'Samples in the specified levels are retained.' ,
                "exclude" = 'Samples in the specified levels are excluded.'
                ),
            type='character',
            allowed=c('include','exclude'),
            value='include',
            max_length=1
        ),

        levels=entity(name='Levels to filter by',
            description='The level name(s) for filtering.',
            type='character',
            value=character(0)
        ),

        factor_name=ents$factor_name
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("filter_vmeta","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        vmeta=D$variable_meta
        x=D$data
        if (opt$mode=='exclude') {
            out=vmeta[[opt$factor_name]] %in% opt$levels
        } else if (opt$mode=='include') {
            out=!(vmeta[[opt$factor_name]] %in% opt$levels)
        } else {
            stop('mode must be "include" or "exclude"')
        }
            # Handle NA values separately
        if (is.na(opt$levels)) {
              out = out | is.na(vmeta[[opt$factor_name]])
        }
        D=D[,!out]
        # drop excluded levels from factors
        D$variable_meta=droplevels(D$variable_meta)
        output_value(M,'filtered')=D
        return(M)
    }
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("filter_vmeta","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("filter_vmeta","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
    }
)
