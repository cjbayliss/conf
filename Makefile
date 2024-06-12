.PHONEY: default

default:
	@for i in $$(find $(PWD)/.???* -type d ! -path '*/.git*'); do \
		mkdir -pv "$(HOME)$${i#$(PWD)}"; \
	done
	@for i in $$(find $(PWD)/.???* -type f ! -path '*/.git*'); do \
		dest="$(HOME)$${i#$(PWD)}"; \
		[ -f "$$dest" ] && [ ! -L "$$dest" ] && mv -v "$$dest" "$$dest.bak"; \
		[ -f "$$dest" ] && rm "$$dest"; \
		[ ! -L "$$dest" ] && ln -sv $$i $$dest; \
	done
	@test -f "$(HOME)/.bashrc" && rm "$(HOME)/.bashrc"
	@ln -sv "$(HOME)/.profile" "$(HOME)/.bashrc"
