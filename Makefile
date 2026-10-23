.PHONY: default

default:
	@for i in $$(find $(PWD)/.???* -type d ! -path '*/.git*'); do \
		mkdir -pv "$(HOME)$${i#$(PWD)}"; \
	done
	@for i in $$(find $(PWD)/.???* -type f ! -path '*/.git*' ! -path '$(PWD)/.docker/config.json'); do \
		dest="$(HOME)$${i#$(PWD)}"; \
		[ -f "$$dest" ] && [ ! -L "$$dest" ] && mv -v "$$dest" "$$dest.bak"; \
		[ -f "$$dest" ] && rm "$$dest"; \
		[ ! -L "$$dest" ] && ln -sv $$i $$dest; \
	done
	@pinentry=$$(command -v pinentry-tty); \
	if [ -n "$$pinentry" ]; then \
		mkdir -pv "$(HOME)/.gnupg"; \
		touch "$(HOME)/.gnupg/gpg-agent.conf"; \
		if grep -q '^pinentry-program' "$(HOME)/.gnupg/gpg-agent.conf"; then \
			sed "s|^pinentry-program .*|pinentry-program $$pinentry|" "$(HOME)/.gnupg/gpg-agent.conf" > "$(HOME)/.gnupg/gpg-agent.conf.tmp" \
			&& mv -v "$(HOME)/.gnupg/gpg-agent.conf.tmp" "$(HOME)/.gnupg/gpg-agent.conf"; \
		else \
			echo "pinentry-program $$pinentry" >> "$(HOME)/.gnupg/gpg-agent.conf"; \
		fi; \
	else \
		echo "pinentry-tty not found; gpg-agent.conf left unchanged"; \
	fi
	@mkdir -pv "$(HOME)/.docker"
	@[ -L "$(HOME)/.docker/config.json" ] && rm -v "$(HOME)/.docker/config.json" || true
	@if brew_prefix=$$(brew --prefix 2>/dev/null); then \
		sed "s|__BREW_PREFIX__|$$brew_prefix|g" "$(PWD)/.docker/config.json" > "$(HOME)/.docker/config.json.tmp" \
		&& mv -v "$(HOME)/.docker/config.json.tmp" "$(HOME)/.docker/config.json"; \
	else \
		echo "brew not found; leaving $(HOME)/.docker/config.json unchanged"; \
	fi
	@if [ "$$(uname)" = "Darwin" ] && [ -d "/Applications/Firefox.app" ]; then \
		mkdir -pv "/Applications/Firefox.app/Contents/Resources/distribution"; \
		cp -v "$(PWD)/firefox-policies.json" "/Applications/Firefox.app/Contents/Resources/distribution/policies.json"; \
	elif [ "$$(uname)" = "Darwin" ]; then \
		echo "Firefox.app not found; policies not installed"; \
	fi
	@if [ -f "$(HOME)/.bashrc" ] || [ -L "$(HOME)/.bashrc" ]; then \
		rm -v "$(HOME)/.bashrc"; \
	fi
	@ln -sv "$(HOME)/.profile" "$(HOME)/.bashrc"
	@if [ -f "$(HOME)/.zshrc" ] || [ -L "$(HOME)/.zshrc" ]; then \
		rm -v "$(HOME)/.zshrc"; \
	fi
	@ln -sv "$(HOME)/.profile" "$(HOME)/.zshrc"
