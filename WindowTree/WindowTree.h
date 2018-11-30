#include<Windows.h>
#include <map>

// In case I wan to use a different map, like std::unordered_map.
// NOTE: Seems that std::unordered_map is slower than std::map in this use case.
template <typename...Ts>
using my_map = std::map<Ts...>;

struct node_t;

enum window_type_e {
	invalid,        // HWND specified by user, but is invalid.
	special,        // Special window (desktop or root).  Can't be reached by window enumeration.
	message,        // Message window.
	top_level,      // Top level window.
	not_top_level,  // Not a top level window.
	user_specified, // HWND specified by user and was not found before.
	not_enumable,   // Other window that can't be reached by window enumeration.
	brute_force,    // Specified by using brute force method (iterating over each possible HWND value).
};
std::wostream& operator<<(std::wostream& os, window_type_e status);

enum verify_correctly_parented_e {
	correct,               // Correctly parented
	parent_node_not_found, // Parent node not found
	parent_was_null,       // Parent node was null and not expected
	parent_was_different   // Parent node was different than expected
};
std::wostream& operator<<(std::wostream& os, verify_correctly_parented_e value);

using hwnd_to_children_ptr_t = my_map<HWND, node_t*>;

// Node to handle information about it.
struct node_t
{
	// PROPERTIES

	// Type of window
	window_type_e m_eType = invalid;

	// HWND that this window is referencing
	HWND m_hCurrent = nullptr;

	// Parent node where m_pParent->m_hCurrent == ::GetParent(m_hCurrent),
	// unless m_pParent is nullptr, then ::GetParent(m_hCurrent) == nullptr
	node_t* m_pParent = nullptr;

	// HWND to child nodes (node_t*)
	hwnd_to_children_ptr_t m_hwnd_to_child_node;

	// Used to mark node for operations where don't want to duplicate 
	// operations done.
	bool m_bMarked = false;

	// Used to count how many handles have been outputted.
	static size_t outputted_handle_count;

	// Process id
	DWORD m_pid = 0;
	// Thread id
	DWORD m_tid = 0;
	// Exe name (uses cache so as not to look it up all the time and share
	// storage)
	std::wstring const* m_pExe_name = nullptr;

	// Exe name (uses cache so can share storage)
	std::wstring const* m_pClass_name = nullptr;
	// Text will most likely be different so, no cache required.
	std::wstring m_text;

	// Is the window visible
	bool m_bVisible;

	// Rectangle for window
	RECT m_rect;

	// General value
	int value = 0;

	// METHODS

	node_t(node_t const&) = delete;
	node_t& operator=(node_t const&) = delete;

	// Constructor - specifies what type it is and will add other window info.
	node_t(HWND hCurrent, window_type_e type);
	
	// Create a node with no info in it except that it is a parent to pChild
	node_t(HWND hCurrent, node_t* pChild);

	// Does the same as constructor with same parameters
	void add_info(HWND hCurrent, window_type_e type);

	// Used to set attributes of node to HWND
	void set_other_attributes();

	// Verifies that node is parented correctly
	verify_correctly_parented_e verify_correctly_parented() const;

	// Adds child node to this node.
	void add_child(node_t* pChild);

	// Outputs nodes and children
	void output_node_and_children(std::wostream& os, int indent = 0);

	// Depth of tree.  Assumes that there is only one.
	static size_t m_nMax_depth;

	// Gets the root node of this node.
	node_t* get_root();

	// Gets the depth from root to deepest leaf
	size_t max_depth_from_root();

	// Finds the depth from this node to the deepest leaf
	size_t max_depth_from_here() const;
};

