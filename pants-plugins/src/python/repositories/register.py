from packend.jvm.repository import Repository
import os

public_repo = Repository(
	name = "public",
	url = "TODO",
	push_db_basedir = os.path.join("build-support", "ivy", "pushdb")
)
