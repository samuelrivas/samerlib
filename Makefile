REBAR_REPO = https://github.com/basho/rebar.git
REBAR_VERSION = 2.0.0
REBAR_REPO_DIR = rebar
REBAR = $(REBAR_REPO_DIR)/rebar

$(REBAR): $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); ./bootstrap

$(REBAR_REPO_DIR):
	git clone $(REBAR_REPO) $(REBAR_REPO_DIR)
