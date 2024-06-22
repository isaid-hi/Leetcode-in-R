# Problem 1: Two sum ------------------------------------------------------

# Given an array of integers nums and an integer target, return indices of the
# two numbers such that they add up to target. You may assume that each input
# would have exactly one solution, and you may not use the same element twice.
# You can return the answer in any order.

#' Example 1:
#' Input: nums = [2,7,11,15], target = 9
#' Output: [0,1]
#' Explanation: Because nums[0] + nums[1] == 9,we return [0, 1].
#' 
#' Example 2: Input: nums = [3,2,4], target = 6
#' Output: [1,2]
#' 
#' Example 3: Input: nums = [3,3], target = 6
#' Output: [0,1]
#' 
#' Constraints:
#' `2 <= nums.length <= 10^4`
#' `-10^9 <= nums[i] <= 10^9`
#' `-10^9 <= target <= 10^9`
#' Only one valid answer exists.
#' 
#' Follow-up: Can you come up with an algorithm
#' that is less than O(n2) time complexity?


two_sum <- function(nums, target) {
  # Loop thru all of the integers in nums
  # Store a complement to the target for each integers in nums
  # check whether the complement[i] 
  
  complement <- target - nums
  
  # Looping
  for (i in seq_along(complement)) {
    if (complement[i] %in% nums) {
      nth <- which(nums == complement[i])
      return(c(i, nth))
    }
  }
}

two_sum(nums, target)
# 
# nums <- sample(-10^9:10^9, sample(2:10^4, 1), replace = FALSE)
# target <- sample(-10^9:10^9, 1)
# which((target - nums) %in% nums)
# nums
# target

nums <- sample(1:10, sample(3:20, 1), replace = TRUE)
target <- sample(1:10, 1)
nums
target
two_sum(nums, target)

# nums <- c(2, 7, 11, 5)
# target <- 9
# complement <- integer()
# for (i in seq_along(nums)) {
#   if (complement[i] %in% nums) {
#     nth <- which(nums == complement[i])
#     return(c(i, nth))
#   }
# }
# 
# complement[1] %in% nums
# nums[which(nums == complement[1])]
# c(i, which(nums == complement[1]))
# 
# 
# print(nums)
# print(target)
# 
# target - nums
