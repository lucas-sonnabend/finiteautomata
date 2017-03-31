from pants.backend.jvm.repository import Repository
from pants.build_graph.build_file_aliases import BuildFileAliases

import os

public_repo = Repository(
	name = "public",
	url = "TODO",
	push_db_basedir = os.path.join("build-support", "ivy", "pushdb"))

def build_file_aliases():
	return BuildFileAliases(
		objects={
			'public': public_repo,
		  },
	)
