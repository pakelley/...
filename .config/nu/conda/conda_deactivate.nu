def conda-deactivate [] {
	let-env PATH = $nu.env.CONDA_OLD_PATH
	unlet-env CONDA_PROMPT_MODIFIER
	unlet-env CONDA_PREFIX
	unlet-env CONDA_SHLVL
	unlet-env CONDA_DEFAULT_ENV
	unlet-env CONDA_OLD_PATH
}

