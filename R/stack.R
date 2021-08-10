#' A simple stack structure and methods
#'
#' @description A simple stack data structure in R, with supporting of assiocated
#' methods, like push, pop and others.
#' @export
#' @return an R6 class object
#' @examples
#' my_stack <- simepleStack$new()
#' # check length
#' my_stack$len()
#' # add some thing
#' my_stack$push(list(1, 2, 3))
#' # print current stack
#' str(my_stack$get())
#' # check length
#' my_stack$len()
#' # add before the current first
#' my_stack$push(list(0), after = 0)
#' # print current stack
#' str(my_stack$get())
#' # pop one item
#' my_stack$pop()
#' # print current stack
#' str(my_stack$get())
#' # pop one item from the tail
#' my_stack$pop(tail = TRUE)
#' # print current stack
#' str(my_stack$get())
#' # pop more than one items
#' my_stack$pop(2)
#' # print current stack
#' str(my_stack$get()) # nothing left
simepleStack <- R6::R6Class(
    classname = "simepleStack",
    public = list(
        #' @description initialize a new object
        #' @param items list, list of items to add to the initial stack
        #' @param limit int, how many items can be pushed to the stack, default is
        #' unlimited.
        initialize = function(items = list(), limit = Inf){
            stopifnot(is.list(items))
            private$stack = self$push(items)
        },
        #' @description returns current length of the stack
        len = function(){
            length(private$stack)
        },
        #' @description returns the full current stack of **all items**
        get = function() {
            return(private$stack)
        },
        #' @description remove **all items** in current stack
        clear = function(){
            private$stack <- list()
        },

        #' @description add item(s) to the stack
        #' @param items list, list of items to add to the stack
        #' @param after int, which position to push items after, default is after
        #' the current last item. 0 will be before the first item.
        push = function(items, after = self$len()) {
            stopifnot(is.list(items))
            if(!is.numeric(after)) stop("Push position must be numeric")
            if(length(after) != 1) stop("Push position cannot be more than 1 number")
            if(after > self$len())  stop("Push position cannot be more than current stack length")
            if(after < 0)  stop("Push position cannot be less than 0")
            private$stack <- append(private$stack, items, after)
        },
        #' @description remove item(s) from the stack and return as results
        #' @param len int, how many items to pop from stack, default is 1 item a time.
        #' @param tail bool, to pop in the reverse order (from the last item)? Default
        #' is `FALSE`, pop from the top (first item).
        pop = function(len=1, tail = FALSE){
            stopifnot(is.logical(tail) && length(tail) == 1)
            if(!is.numeric(len)) stop("Pop numbers must be numeric")
            if(len < 1)  stop("Pop length cannot be less than 1")
            stack_len <- self$len()
            if(len > stack_len) stop("Pop length cannot be more than stack length")
            len <- as.integer(len)
            if (tail) {
                stack_len <- self$len()
                pop_index <- if(len == 1) stack_len else seq(stack_len, stack_len - len)
                pop_index <- pop_index[pop_index != 0]
            } else {
                pop_index <- seq_len(len)
            }
            pop_items <- private$stack[pop_index]
            private$stack <- private$stack[-pop_index]
            pop_items
        }
    ),
    private = list(
        stack = list()
    )
)

#' history stack structure and methods
#'
#' @description Some methods for a history stack data structure. It can store history
#' of certain repeating actions. For example, building the back-end of a file/image
#' editor, allow undo/redo actions.
#' @details
#' 1. If the stack reaches the limit and you are trying to add more history, the
#'    first history step will be removed , all history will be shift to the left
#'    by one step and finally add the new step to the end.
#' 2. When history returning methods are called, like the `get()`, `forward()`, `backward()`
#'    methods, it will not directly return the item saved, but a list, contains 4
#'    components: 1. item, the actual item stored; 2. pos, current posistion value;
#'    3. first, boolean value, if this history is stored on the first position of stack;
#'    4. last, boolean value, if this history is stored on the last position of stack;
#' 3. If you `forward` beyond last step, or `backward` to prior the first
#'    step, it will be stopped with errors.
#' 4. Starting  history stack with no initial history will return a special
#'    stack, where the `pos = 0`, `len = 0`, `first = TRUE`, and `last = TRUE`.
#'    This means you cannot move forward or backward. When you `get()`, it will be
#'    an empty list `list()`. After adding any new history, `pos` will never be
#'    0 again, it will always be a larger than 0 value.
#' @return an R6 class object
#' @export
#' @examples
#' his <- historyStack$new()
#' # add some history
#' his$add(1)
#' his$add(2)
#' his$add(3)
#' his$add(4)
#' his$add(5)
#' # check status
#' his$status()
#' # get item at current history position
#' his$get()
#' # go back to previous step
#' his$backward()
#' # going back to step 2
#' his$backward()
#' his$backward()
#' # going forward 1 step tp step 3
#' his$forward()
#' # check current status
#' his$status()
#' # adding a new step at position 3 will remove the old step 4,5 before adding
#' his$add("new 4")
#' # only 3 steps + 1 new step = 4 steps left
#' his$status()
historyStack <- R6::R6Class(
    classname = "historyStack",
    public = list(
        #' @description create the history object
        #' @param items list, initial history step items to store on start
        #' @param limit int, how many history steps can be stored in the stack,
        #' default 25 steps
        #' @param verbose bool, print some verbose message?
        initialize = function(items = NULL, limit = 25, verbose = TRUE){
            if(is.null(items)) items <- list()
            stopifnot(is.list(items))
            stopifnot(is.numeric(limit))
            stopifnot(is.logical(verbose) && length(verbose) == 1)
            if(limit < 1) stop("Cannot create a history stack with limit less than 1.")
            if(limit == Inf) stop("Limit cannot be Inf.")

            limit <- as.integer(limit)
            private$limit = limit
            if(length(items) > limit) stop("Initial items length larger than limit.")

            item_len <- length(items)
            private$stack <- append(private$stack, items)

            private$len <- private$pos <- item_len
            private$last <- if(private$pos >= 0) TRUE
            private$first <- if(private$pos <= 1) TRUE else FALSE
            private$verbose <- verbose

            if(private$verbose) message("Created a history stack which can record  ", limit, " steps")
            invisible(self)
        },
        #' @description clear all history steps in the stack
        clear = function(){
            limit <- private$limit
            verbose <- private$verbose
            self$initialize(limit = limit, verbose = verbose)

            if(private$verbose) message("Stack clear")
            invisible(self)
        },
        #' @description retrieve the history from a certain position in the stack
        #' @param pos int, which position to get the history from, default is current
        #' step.
        get = function(pos = private$pos){
            stopifnot(is.numeric(pos) && length(pos) == 1)
            if(pos == Inf) stop("Position cannot be Inf.")
            pos <- as.integer(pos)
            if(pos > private$len) stop("Position is larger than current max history storage")
            item <- if(pos == 0) list() else private$stack[[pos]]
            list(
                item = item,
                pos = pos,
                first = private$first,
                last = private$last
            )
        },
        #' @description get current step position in the history stack
        getPos = function(){
            private$pos
        },
        #' @description print out some status of the stack
        #' @return returns a list of `pos`: current position (int); `len`: current
        #' length of the history stack (int); `limit`: history stack storing limit (int);
        #' `first`: is current step position the first of the stack (bool);
        #' `last`: is current step position the last of the stack (bool)
        status = function(){
            list(
                pos = private$pos,
                len = private$len,
                limit = private$limit,
                first = private$first,
                last = private$last
            )
        },
        #' @description move one step forward in the history stack and return item
        #' in that position
        forward = function(){
            if(private$last) {
                if(private$verbose) warning("Already the last history, cannot move forward")
                return(NULL)
            }
            inc(private$pos)
            private$last <- if(private$pos == private$len) TRUE else FALSE
            private$first <- if(private$pos == 1) TRUE else FALSE

            self$get(private$pos)
        },
        #' @description move one step backward in the history stack and return item
        #' in that position
        backward = function(){
            if (private$first){
                if(private$verbose) warning("Already the first history, cannot move backward")
                return(NULL)
            }
            inc(private$pos, -1)
            private$first <- if(private$pos == 1) TRUE else FALSE
            private$last <- if(private$pos == private$len) TRUE else FALSE

            self$get(private$pos)
        },
        #' @description Add an item to the history and move one step forward
        #' @param item any object you want to add to the stack. Everything store
        #' in the item will be moved into a list, so even if item may be something
        #' length > 1, it will still be treated as a single item and single history
        #' step.
        #' @details If current position is not the last position, and when a new
        #' step item is added to the stack, all history records (items) after current
        #' position will be removed before adding the new history item.
        add = function(item) {
            pos <- private$pos
            stack <- private$stack
            item <- list(item)
            if (pos != private$limit) {
                stack <- if(length(stack)) append(stack[seq_len(pos)], item) else item
                inc(pos)
            } else {
                stack <- append(stack[-1], item)
            }
            private$stack <- stack
            private$len <- length(stack)
            private$pos <- pos
            private$first <- if(pos == 1) TRUE else FALSE
            private$last <- TRUE
            if(private$verbose) message("Added one item to position ", pos)
            invisible(self)
        }
    ),
    private = list(
        stack = list(),
        pos = 0,
        len = 0,
        limit = 25,
        first = TRUE,
        last = FALSE,
        verbose = NULL
    )
)
