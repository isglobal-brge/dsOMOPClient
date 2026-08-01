test_that("omop_population validates explicit recurrent-episode policies", {
  p <- omop_population(
    id = "eligible",
    filters = omop_filter_age_group("30-39"),
    episode_policy = "ANY_EPISODE"
  )
  expect_identical(p$episode_policy, "any_episode")

  expect_error(
    omop_population(episode_policy = "per_episode"),
    "episode_policy must be one of"
  )
  expect_error(
    omop_population(episode_policy = c("first_episode", "last_episode")),
    "episode_policy must be one of"
  )
  expect_error(
    omop_population(episode_policy = NA_character_),
    "episode_policy must be one of"
  )
  expect_error(
    omop_population(
      id = "either",
      union = c("a", "b"),
      episode_policy = "any_episode"
    ),
    "set-op population.*episode_policy"
  )
})

test_that("recipe_to_plan transports population and base episode policies", {
  eligible <- omop_population(
    id = "eligible",
    filters = omop_filter_age_group("30-39"),
    episode_policy = "all_episodes"
  )
  plan <- recipe_to_plan(omop_recipe(populations = eligible))
  expect_identical(
    plan$populations$eligible$episode_policy,
    "all_episodes"
  )

  base <- omop_population(
    id = "base",
    filters = omop_filter_age_group("30-39"),
    cohort_definition_id = 42L,
    episode_policy = "first_episode"
  )
  base_plan <- recipe_to_plan(omop_recipe(populations = base))
  expect_identical(
    base_plan$populations$base$episode_policy,
    "first_episode"
  )
  expect_identical(base_plan$cohort$episode_policy, "first_episode")
})

test_that("episode_policy survives JSON, YAML, and generated-code round trips", {
  base <- omop_population(
    id = "base",
    filters = omop_filter_age_group("30-39"),
    cohort_definition_id = 42L,
    episode_policy = "last_episode"
  )
  recipe <- omop_recipe(populations = base)

  from_json <- recipe_import_json(recipe_export_json(recipe))
  expect_identical(from_json$populations$base$episode_policy, "last_episode")

  if (requireNamespace("yaml", quietly = TRUE)) {
    from_yaml <- recipe_import_yaml(recipe_export_yaml(recipe))
    expect_identical(from_yaml$populations$base$episode_policy, "last_episode")
  }

  code <- recipe_to_code(recipe)
  rebuilt <- eval(parse(text = paste(code, collapse = "\n")))
  expect_identical(rebuilt$populations$base$episode_policy, "last_episode")
  expect_identical(
    recipe_to_plan(rebuilt)$populations$base$episode_policy,
    "last_episode"
  )
})

test_that("default populations do not gain episode-policy transport fields", {
  p <- omop_population()
  expect_null(p$episode_policy)
  expect_null(
    recipe_to_plan(omop_recipe())$populations$base$episode_policy
  )
})
