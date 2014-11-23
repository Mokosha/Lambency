module Lambency.Shader.Optimization (
  module Lambency.Shader.Optimization.RemoveUnused
) where

--------------------------------------------------------------------------------
-- !TODO! Eventually create an 'optimize' function that takes a program AST and
-- returns an optimized AST. Things to do with the optimizer:
--   1. Remove unused variables
--   2. Collapse const expressions
--   3. Identify common patterns and reduce their overhead (e.g. if you swizzle
--      a swizzle, no need to swizzle twice)

import Lambency.Shader.Optimization.RemoveUnused
