#' Create a file <name_file>.txt
#' 
#'  @param variable where is the file to output
#'  @param Name of the output file
#'    
#'  @return the file ouput   
#'      
#'  @export

output_file  =function(fic,output_file,format)
    {
    if(format == "csv")
        {
        write.csv(fic,file='output_file')
        return('output_file')
        }
        else 
            capture.output(fic,file = 'output_file')
            return('output_file')
    }
