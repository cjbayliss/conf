-- Windows live in a centred vertical strip: the focused one fills the usable
-- height, its neighbours sit beside it, and the rest wait off screen.

-- Sizes are in points; RESIZE_STEP is a fraction of the usable width.
local PADDING = 6
local RESIZE_STEP = 0.1
local MIN_WIDTH = 100

-- Dialogs, sheets and palettes would only get in the way of the strip.
local function isStandardWindow(window)
    local subrole = window:subrole()
    return subrole == nil or subrole == "AXStandardWindow"
end

local function fittedFrame(window, widthFraction)
    local screenFrame = (window:screen() or hs.screen.mainScreen()):frame() -- usable area, menu bar and dock excluded
    local availableWidth = screenFrame.w - 2 * PADDING
    local width = widthFraction and availableWidth * widthFraction or math.min(window:frame().w, availableWidth)

    return hs.geometry.rect(
        screenFrame.x + PADDING + (availableWidth - width) / 2,
        screenFrame.y + PADDING,
        width,
        screenFrame.h - 2 * PADDING
    )
end

local function fitWindow(window, widthFraction)
    if not window or not isStandardWindow(window) or window:isFullScreen() then
        return nil
    end

    local frame = fittedFrame(window, widthFraction)
    window:setFrame(frame)
    return frame
end

local windows = hs.window.filter.new():setOverrideFilter({ visible = true, hasTitlebar = true, fullscreen = false })

local function byScreenPosition(windowA, windowB)
    local frameA, frameB = windowA:frame(), windowB:frame()

    if frameA.x ~= frameB.x then
        return frameA.x < frameB.x
    end
    if frameA.y ~= frameB.y then
        return frameA.y < frameB.y
    end
    return (windowA:id() or 0) < (windowB:id() or 0)
end

-- Order is remembered, not read from frames: once the windows sit in the
-- strip their positions say nothing about where they came from, and a stable
-- order lets cycling back retrace the same path.
local order = {}
local lastFocusedId

local function visibleCandidates()
    local visible = {}
    for _, window in ipairs(hs.window.orderedWindows()) do
        if isStandardWindow(window) then
            visible[#visible + 1] = window
        end
    end

    if #visible == 0 then
        return nil
    end

    table.sort(visible, byScreenPosition)
    return visible
end

local function indexOf(ids, id)
    for position, knownId in ipairs(ids) do
        if knownId == id then
            return position
        end
    end
end

local function filterVisible(ids, visible)
    local present = {}
    for _, window in ipairs(visible) do
        present[window:id()] = true
    end

    local result = {}
    for _, id in ipairs(ids) do
        if present[id] then
            result[#result + 1] = id
        end
    end
    return result
end

-- A newcomer goes right after afterId, so it shows up next to the window
-- that had focus instead of jumping to the end. Only a close removes an id:
-- focus and visibility changes must never reorder anything.
local function syncOrder(visible, afterId)
    local ids = {}
    local known = {}
    for _, id in ipairs(order) do
        ids[#ids + 1] = id
        known[id] = true
    end

    for _, window in ipairs(visible) do
        local id = window:id()
        if not known[id] then
            known[id] = true
            table.insert(ids, (indexOf(ids, afterId) or #ids) + 1, id)
            afterId = id
        end
    end

    order = ids
    return filterVisible(ids, visible)
end

local function orderedCandidates(afterId)
    local visible = visibleCandidates()
    if not visible then
        return nil
    end

    return syncOrder(visible, afterId)
end

-- Park a window at the screen edge but keep a sliver visible, so macOS still
-- treats it as on screen and lets us focus it later.
local function parkWindow(window, side, screen)
    local frame = window:frame()
    local screenFrame = screen:frame()
    local xPosition = side == "left" and screenFrame.x - frame.w + 1 or screenFrame.x + screenFrame.w - 1
    window:setFrame(hs.geometry.rect(xPosition, frame.y, frame.w, frame.h))
end

-- Neighbours sit right beside the focused window; everything further away is
-- parked so it cannot peek out from behind them.
local function layoutWindows(ids, focusIndex)
    local focused = hs.window.get(ids[focusIndex])
    local target = fitWindow(focused)
    if not target then
        return
    end

    local screen = focused:screen() or hs.screen.mainScreen()

    for position, id in ipairs(ids) do
        if position ~= focusIndex then
            local window = hs.window.get(id)
            if window and not window:isFullScreen() then
                local frame = window:frame()
                local distance = position - focusIndex
                if math.abs(distance) == 1 then
                    local xPosition = distance < 0 and target.x - PADDING - frame.w or target.x + target.w + PADDING
                    window:setFrame(hs.geometry.rect(xPosition, frame.y, frame.w, frame.h))
                else
                    parkWindow(window, distance < 0 and "left" or "right", screen)
                end
            end
        end
    end
end

local function layoutAround(window)
    local ids = orderedCandidates(lastFocusedId)
    if not ids then
        return
    end

    local index = indexOf(ids, window:id())
    if index then
        layoutWindows(ids, index)
    end
end

local function onWindowFocused(window)
    -- Desktop and panel windows have no place in the strip.
    if not window or not isStandardWindow(window) then
        return
    end

    -- Lay out before recording the focused window: a newcomer must anchor
    -- after the window that had focus until now.
    layoutAround(window)
    lastFocusedId = window:id()
end

-- Focus the window to the left of a closed one, after a short delay so macOS
-- has finished its own focus shuffle and our choice sticks.
local function onWindowDestroyed(window)
    local closedId = (window and window:id()) or lastFocusedId
    local index = closedId and indexOf(order, closedId)

    -- wrap around to the rightmost window
    local leftId = index and order[(index - 2) % #order + 1]
    if index then
        table.remove(order, index)
    end

    if #order == 0 then
        lastFocusedId = nil
        return
    end
    if not leftId then
        return
    end

    hs.timer.doAfter(0.05, function()
        local leftWindow = hs.window.get(leftId)
        if not leftWindow then
            return
        end

        lastFocusedId = leftId
        leftWindow:focus()
        layoutAround(leftWindow)
    end)
end

-- Seed the order from what is already on screen so a reload does not lose it.
local seeded = visibleCandidates()
if seeded then
    syncOrder(seeded)
end

local frontmost = hs.window.focusedWindow()
if frontmost and isStandardWindow(frontmost) then
    lastFocusedId = frontmost:id()
end

windows:subscribe(hs.window.filter.windowFocused, onWindowFocused)
windows:subscribe(hs.window.filter.windowDestroyed, onWindowDestroyed)

local function focusContext()
    local focused = hs.window.focusedWindow()
    local focusedId = focused and focused:id()
    local ids = orderedCandidates(focusedId)
    return ids, ids and indexOf(ids, focusedId)
end

local function cycleWindow(offset)
    local ids, current = focusContext()
    if not ids then
        return
    end

    -- Nothing focused, or focus outside the order (the desktop, say): start
    -- just before the end we are moving away from.
    current = current or (offset > 0 and 0 or #ids + 1)

    local targetIndex = current + offset
    local target = ids[targetIndex] and hs.window.get(ids[targetIndex])
    if not target then
        return
    end

    target:focus()
    layoutWindows(ids, targetIndex)
end

local function swapWindow(offset)
    local ids, current = focusContext()
    if not current then
        return
    end

    local partnerIndex = current + offset
    local partnerId = ids[partnerIndex]
    if not partnerId then
        return
    end

    -- Swap in the full order too, so parked windows keep their slots.
    local currentSlot, partnerSlot = indexOf(order, ids[current]), indexOf(order, partnerId)
    if currentSlot and partnerSlot then
        order[currentSlot], order[partnerSlot] = order[partnerSlot], order[currentSlot]
    end

    ids[current], ids[partnerIndex] = partnerId, ids[current]
    layoutWindows(ids, partnerIndex)
end

local function fitFocused(widthFraction)
    local window = hs.window.focusedWindow()
    if fitWindow(window, widthFraction) then
        layoutAround(window)
    end
end

local function resizeWindow(direction)
    local window = hs.window.focusedWindow()
    if not window then
        return
    end

    local availableWidth = (window:screen() or hs.screen.mainScreen()):frame().w - 2 * PADDING
    local width = math.min(window:frame().w, availableWidth) + direction * availableWidth * RESIZE_STEP

    fitFocused(math.max(MIN_WIDTH, math.min(availableWidth, width)) / availableWidth)
end

local CTRL_ALT = { "ctrl", "alt" }
local CTRL_ALT_SHIFT = { "ctrl", "alt", "shift" }

hs.hotkey.bind(CTRL_ALT, "left", function()
    cycleWindow(-1)
end)

hs.hotkey.bind(CTRL_ALT, "right", function()
    cycleWindow(1)
end)

hs.hotkey.bind(CTRL_ALT_SHIFT, "left", function()
    swapWindow(-1)
end)

hs.hotkey.bind(CTRL_ALT_SHIFT, "right", function()
    swapWindow(1)
end)

hs.hotkey.bind(CTRL_ALT, "=", function()
    resizeWindow(1)
end)

hs.hotkey.bind(CTRL_ALT, "-", function()
    resizeWindow(-1)
end)

hs.hotkey.bind(CTRL_ALT, "r", function()
    fitFocused(0.85)
end)

hs.hotkey.bind(CTRL_ALT, "m", function()
    fitFocused(1)
end)

hs.hotkey.bind(CTRL_ALT_SHIFT, "r", hs.reload)
