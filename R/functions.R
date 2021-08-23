#' Compute the index of a word, using Knuth-Morris-Pratt algorithm
#'
#' This function compute the index for the KMP algorithm. Inside the function the index
#' will be 1-based, but the output will be 0-based as described in the wikipedia page
#'
#' @param pattern   The word for which the index will be created
#' @return A vector of integers, that will represent the index for the given word.
#' @author Lorenzo Brochier\cr e-mail: <lorenzomathieu.brochier@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm}\cr
#' @seealso \code{\link{KMP}}\cr
#' @export
kmp_index = function(pattern){
    # checking the correctness of the input
    if(typeof(pattern) != 'character' || pattern == '' )
    {
        stop("pattern is not of class 'character' or is an empty string")
    }
    # transforming string to vector in order to simplify operations
    pattern = strsplit(pattern, split = '')[[1]]

    # we first create the index vector filled with a value that will never
    # be used to compute the actual values of the index, in order to spot if
    # something went wrong in the filling step
    index = rep(-2, length(pattern)+1)

    # defining the indices that will be used to iterate through the pattern
    pos = 2  # the position we are computing in 'index'

    cnd = 1  # the index that will show the position of the character in 'pattern'
             # from which the comparison with S in KMP() restarts after a mismatch;
             # called 'restart-position' in following code

    # setting the first element to 0 due to algorithmic reasons
    index[1] = 0

    # the actual algorithm
    while(pos <= length(pattern))  # until the end of 'pattern' is not reached...
    {
      if(pattern[pos] == pattern[cnd])  # if the 2 charachter are equal they will
                                        # have the same index
      {
          index[pos] = index[cnd]  # setting the index
      }else
      {
          index[pos] = cnd  # here we are just assigning the restart-position for character
                            # in position 'pos'

          while(cnd >= 1 && pattern[pos] != pattern[cnd])
          {cnd = index[cnd]}

      }

        pos = pos + 1  # going to the next character of 'pattern'
        cnd = cnd + 1  # increasing the restart-position or just setting it to 1
    }

    # here we are at the end of 'pattern' and we are setting
    # the restart-position at the end of 'pattern'
    index[pos] = cnd
    return(index-1) # the '-1' is needed to have the index as described in the wikipedia page
}


#' Gives vector of positons in text S in which P is found
#'
#' This function compute a vector representing positions at which a substring P is present
#' in a text S. if present, it recognizes spaces as part of the substring P. The search is
#' done through the Knuth-Morris-Pratt algorithm
#'
#' @param P   The substring/pattern to search in text S
#' @param S   The text in which P will be searched
#' @return A vector of integers, that will represent the positions
#' in S where a complete match is found. These positions represent the
#' position of the first character of P in S.If no occurence is found NULL is given.
#' @author Lorenzo Brochier\cr e-mail: <lorenzomathieu.brochier@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm}\cr
#' @seealso \code{\link{kmp_index}}\cr
#' @export
KMP = function(P, S){
    # checking the correctness of the input
    if(typeof(S) != 'character' || S == '' )
    {
        stop("Text S is not of class 'character' or is an empty string")
    }

    # compute the index of P
    P_idx = kmp_index(P)+1  # we add 1 since the output index of kmp_index is zero-based but R is not

    # transforming strings to vectors in order to simplify operations
    S = strsplit(S, split = '')[[1]]
    P = strsplit(P, split = '')[[1]]

    if(length(P) > length((S)))
    {
        stop("Length of pattern P greater than length of text S .Choose a shorter pattern")
    }

    # defining indices
    j = 1  # the index that for the position of the character in S
    k = 1  # the index for the position of the character in P
    occ = c() # the positions in S at which a complete match is found

    # the actual algorithm
    while(j <= length(S)) # untill the end of the text is not reached...
    {
        if(P[k] == S[j])
        {
            j = j +1  # moving to the next character in S
            k = k +1  # moving to the next character in P
            if(k == length(P) + 1) # this is done when the match is complete
            {
                occ = c(occ, j - k + 1)  # the position of the match is added to the vector
                k = P_idx[k]  # the position in P at which the comparison restarts

            }

        }else  # if the two character are not equal...
            {
                k = P_idx[k]  # the position in P at which the comparison restarts
                if(k < 1) # if the chracter in P is equal to its first character (it means k=0) the
                          # comparison restarts from the first chracter of P and from
                          # character j +1 of S
                {
                    j = j +1
                    k = k +1 # can be substituted with 'k = 1'?
                }
            }
    }
    return(occ)
}
